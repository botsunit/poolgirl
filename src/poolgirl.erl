-module(poolgirl).
-compile({parse_transform, ms_transform}).
-behaviour(gen_server).
-include("../include/poolgirl.hrl").

%% API.
-export([
         start_link/0,
         add_pool/1,
         add_pool/2,
         add_pool/3,
         remove_pool/1,
         remove_pools/1,
         remove_all_pools/0,
         checkout/1,
         checkin/1,
         size/1,
         pools/0,
         assigned/1,
         transaction/2
        ]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
          options,
          workers,
          pools
         }).

-record(worker, {
          pool,
          pid,
          mref,
          assigned = false,
          since = 0
         }).

-record(pool, {
          name,
          supervisor,
          module,
          function,
          args,
          initial_size,
          chunk_size,
          timer,
          max_age,
          max_size,
          clean_interval,
          max_retry,
          retry_interval
         }).

-type mfargs() :: {Module :: atom(), Function :: atom(), Args :: list()}.
-type pool_options() :: #{size => integer(),
                          chunk_size => integer(),
                          max_age => integer(),
                          max_size => integer(),
                          clean_interval => integer(),
                          retry_interval => integer(),
                          allow_empty_pool => true | false,
                          max_retry => integer()}.

% @hidden
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% @doc
% Start a configured pool
% @end
-spec add_pool(atom()) -> {ok, integer()} | {error, term()}.
add_pool(Name) ->
  case doteki:get_env([poolgirl, pools, Name], undefined) of
    undefined ->
      {error, missing_pool_configuration};
    Options ->
      case lists:keyfind(start, 1, Options) of
        {start, {_, _, _} = MFArgs} ->
          add_pool(Name, MFArgs, maps:without([autostart, start],
                                              maps:from_list(Options)));
        _ ->
          {error, missing_mfargs}
      end
  end.

% @equiv add_pool(Name, MFArgs, #{})
-spec add_pool(atom(), mfargs()) -> {ok, integer()} | {error, term()}.
add_pool(Name, MFArgs) ->
  add_pool(Name, MFArgs, #{}).

% @doc
% Create a new pool.
%
% Options :
% <ul>
% <li><tt>size :: integer()</tt> : Minimum pool size (Default : 5).</li>
% <li><tt>chunk_size :: integer()</tt> : Chunk size (Default : 10).</li>
% <li><tt>max_age :: integer()</tt> : Maximum age (in ms) of unused workers before destruction (Default : 120000).</li>
% <li><tt>max_size :: integer()</tt> : Maximum number or worker in the pool (Default: infinity).</li>
% <li><tt>clean_interval :: integer()</tt> : Interval (in ms) between each cleanup (Default : 60000).</li>
% <li><tt>max_retry :: integer()</tt> : Number of new attempts to acquire workers if none is available (Default : 0).</li>
% <li><tt>retry_interval :: integer()</tt> : Interval (in ms) between workers acquisition attempts (Default : 100).</li>
% <li><tt>allow_empty_pool :: true | false</tt> : If this option is set to true and the pool is empty at start, it is removed (Default: false).</li>
% <li><tt>worker_timeout :: integer()</tt> : Integer greater than zero that specifies how many milliseconds to wait for each worker to start (Default: 5000).</li>
% </ul>
%
% <i>Warning</i> : If <tt>max_size =&lt; size + chunk_size</tt> then <tt>max_size</tt> is set to <tt>size + chunk_size</tt>
%
% Example :
% <pre>
% poolgirl:add_pool(test, {my_server, start_link, [{127,0,0,1}, 9092]}, #{size => 2,
%                                                                         chunk_size => 4}).
% </pre>
% @end
-spec add_pool(atom(), mfargs(), pool_options()) -> {ok, integer()} | {error, term()}.
add_pool(Name, MFArgs, Options) ->
  #{worker_timeout := Timeout,
    size := Size,
    chunk_size := _ChunkSize} = gen_server:call(?MODULE, {options, Name, Options}),
  case gen_server:call(?MODULE, {add_pool, Name, MFArgs, Options}, Timeout * Size) of
    {ok, 0} ->
      case maps:get(allow_empty_pool, Options, false) of
        false ->
          remove_pool(Name),
          {error, timeout};
        true ->
          {ok, 0}
      end;
    Other ->
      Other
  end.

% @doc
% Remove an existing pool
% @end
-spec remove_pool(atom()) -> ok | {error, term()}.
remove_pool(Name) ->
  gen_server:call(?MODULE, {remove_pool, Name}).

% @doc
% Remove a list of existing pools
% @end
-spec remove_pools([atom()]) -> ok | [{error, term()}].
remove_pools(Pools) ->
  case lists:filter(fun(E) ->
                        E =/= ok
                    end, [case remove_pool(Pool) of
                            {error, Reason} ->
                              {error, Pool, Reason};
                            Other ->
                              Other
                          end || Pool <- Pools]) of
    [] ->
      ok;
    Other ->
      Other
  end.

% @doc
% Remove all existing pools
% @end
-spec remove_all_pools() -> ok | [{error, term(), term()}].
remove_all_pools() ->
  remove_pools(pools()).

% @doc
% Checkout a worker
%
% Example:
% <pre>
% W = poolgirl:checkout(test).
% </pre>
% @end
-spec checkout(atom()) -> {ok, pid()} | {error, term()}.
checkout(Pool) ->
  case gen_server:call(?MODULE, {retry, Pool}) of
    {ok, MaxRetry, RetryInterval} ->
      checkout(Pool, RetryInterval, MaxRetry + 1);
    Error ->
      Error
  end.
checkout(_, _, 0) ->
  {error, no_available_worker};
checkout(Pool, RetryInterval, N) ->
  case gen_server:call(?MODULE, {checkout, Pool}) of
    {error, no_available_worker} ->
      _ = timer:sleep(RetryInterval),
      checkout(Pool, RetryInterval, N-1);
    Other ->
      Other
  end.

% @doc
% Checkin a worker
%
% Example:
% <pre>
% poolgirl:checkin(W).
% </pre>
% @end
-spec checkin(pid()) -> ok | {error, term()}.
checkin(Pid) ->
  gen_server:call(?MODULE, {checkin, Pid}).

% @doc
% Get a worker size and number of unassigned workers.
%
% Example:
% <pre>
% poolgirl:size(test).
% </pre>
% @end
-spec size(atom()) -> {ok, integer(), integer()} | {error, term()}.
size(Pool) ->
  gen_server:call(?MODULE, {size, Pool}).

% @doc
% Return the list of pools
%
% Example:
% <pre>
% poolgirl:size(test).
% </pre>
% @end
-spec pools() -> [atom()].
pools() ->
  gen_server:call(?MODULE, pools).

% @doc
% Return the list of assigned workers
%
% Example:
% <pre>
% {ok, Workers} = poolgirl:assigned(test).
% </pre>
% @end
-spec assigned(atom()) -> {ok, [pid()]} | {error, term()}.
assigned(Pool) ->
  gen_server:call(?MODULE, {assigned, Pool}).

% @doc
% Checkout a worker from the given pool and execute a function with the worker as parameter.
%
% Example:
% <pre>
% poolgirl:transaction(test, fun(Worker) ->
%   gen_server(Worker, {do, Something})
% end).
% </pre>
% @end
-spec transaction(atom(), fun((Worker :: pid()) -> Result :: term())) -> Result :: term() | {error, term()}.
transaction(Pool, Fun) when is_function(Fun, 1) ->
  case checkout(Pool) of
    {ok, Worker} ->
      try
        erlang:apply(Fun, [Worker])
      catch
        Class:Error ->
          {error, {Class, Error}}
      after
        checkin(Worker)
      end;
    Error ->
      Error
  end.

%% gen_server.

% @hidden
init([]) ->
  {ok, #state{
          options = #{size => doteki:get_env([poolgirl, size], ?INITIAL_SIZE),
                      chunk_size => doteki:get_env([poolgirl, chunk_size], ?CHUNK_SIZE),
                      max_age => doteki:get_env([poolgirl, max_age], ?MAX_AGE),
                      max_size => doteki:get_env([poolgirl, max_size], ?MAX_SIZE),
                      clean_interval => doteki:get_env([poolgirl, clean_interval], ?CLEAN_INTERVAL),
                      retry_interval => doteki:get_env([poolgirl, retry_interval], ?RETRY_INTERVAL),
                      allow_empty_pool => doteki:get_env([poolgirl, allow_empty_pool], ?ALLOW_EMPTY_POOL),
                      worker_timeout => doteki:get_env([poolgirl, worker_timeout], ?WORKER_TIMEOUT),
                      max_retry => doteki:get_env([poolgirl, max_retry], ?MAX_RETRY)},
          pools = ets:new(pools, [private,
                                  {keypos, #pool.name}]),
          workers = ets:new(workers, [private,
                                      {keypos, #worker.pid}])}}.

% @hidden
handle_call({retry, Name}, _From, #state{pools = Pools} = State) ->
  case ets:lookup(Pools, Name) of
    [] ->
      {reply, {error, unknow_pool}, State};
    [#pool{max_retry = MaxRetry, retry_interval = RetryInterval}] ->
      {reply, {ok, MaxRetry, RetryInterval}, State};
    _ ->
      {reply, {error, pool_failed}, State}
  end;
handle_call({add_pool, Name, {Module, Function, Args} = MFArgs, Options},
            _From, #state{pools = Pools,
                          workers = Workers,
                          options = CommonOptions} = State) ->
  case ets:lookup(Pools, Name) of
    [] ->
      PoolOptions = pool_options(Name, Options, CommonOptions),
      Size = maps:get(size, PoolOptions, ?INITIAL_SIZE),
      ChunkSize = maps:get(chunk_size, PoolOptions, ?CHUNK_SIZE),
      MaxAge = maps:get(max_age, PoolOptions, ?MAX_AGE),
      CleanInterval = maps:get(clean_interval, PoolOptions, ?CLEAN_INTERVAL),
      MaxSize = case maps:get(max_size, PoolOptions, ?MAX_SIZE) of
                  infinity ->
                    infinity;
                  Max when Max =< (Size + ChunkSize) ->
                    Size + ChunkSize;
                  Max ->
                    Max
                end,
      MaxRetry = maps:get(max_retry, PoolOptions, ?MAX_RETRY),
      RetryInterval = maps:get(retry_interval, PoolOptions, ?RETRY_INTERVAL),
      case poolgirl_sup:add_pool(Name, MFArgs) of
        {ok, SupervisorPid} ->
          ets:insert(Pools, #pool{name = Name,
                                  supervisor = SupervisorPid,
                                  module = Module,
                                  function = Function,
                                  args = Args,
                                  initial_size = Size,
                                  max_age = MaxAge,
                                  max_size = MaxSize,
                                  clean_interval = CleanInterval,
                                  max_retry = MaxRetry,
                                  retry_interval = RetryInterval,
                                  timer = erlang:send_after(CleanInterval, self(), {clean, Name}),
                                  chunk_size = ChunkSize}),
          {reply, add_workers(Name, State), State};
        Error ->
          {reply, Error, State}
      end;
    [#pool{}] ->
      Size = ets:match(Workers, #worker{pool = Name, _ = '_'}),
      {reply, {ok, length(Size)}, State};
    _ ->
      {reply, {error, pool_failed}, State}
  end;
handle_call({remove_pool, Name}, _From, #state{pools = Pools} = State) ->
  case ets:lookup(Pools, Name) of
    [] ->
      {reply, {error, unknow_pool}, State};
    _ ->
      case poolgirl_sup:remove_pool(Name) of
        ok ->
          _ = ets:delete(Pools, Name),
          {reply, ok, State};
        Error ->
          {reply, Error, State}
      end
  end;
handle_call({checkout, Name}, From, #state{pools = Pools, workers = Workers} = State) ->
  case ets:lookup(Pools, Name) of
    [] ->
      {reply, {error, unknow_pool}, State};
    [#pool{}] ->
      case ets:match(Workers, #worker{assigned = false,
                                      pool = Name,
                                      pid = '$1',
                                      _ = '_'}) of
        [] ->
          _ = add_workers(Name, State),
          {reply, {error, no_available_worker}, State};
        [[Pid]|_] ->
          case erlang:is_process_alive(Pid) of
            true ->
              case ets:update_element(Workers, Pid, {#worker.assigned, true}) of
                true ->
                  _ = add_workers(Name, State),
                  {reply, {ok, Pid}, State};
                false ->
                  {reply, {error, checkout_faild}, State}
              end;
            false ->
              case ets:match(Workers, #worker{mref = '$1', pid = Pid, _ = '_'}) of
                [[MRef]|_] ->
                  _ = erlang:demonitor(MRef),
                  _ = ets:delete(Workers, Pid),
                  handle_call({checkout, Name}, From, State);
                _ ->
                  {reply, {error, checkout_faild}, State}
              end
          end
      end;
    _ ->
      {reply, {error, pool_failed}, State}
  end;
handle_call({checkin, Pid}, _From, #state{workers = Workers} = State) ->
  case ets:lookup(Workers, Pid) of
    [] ->
      {reply, {error, unknow_worker}, State};
    [#worker{assigned = true}|_] ->
      case ets:update_element(Workers, Pid, [{#worker.assigned, false}, {#worker.since, epoch()}]) of
        true ->
          {reply, ok, State};
        false ->
          {reply, {error, checkin_faild}, State}
      end
  end;
handle_call({size, Name}, _From, #state{pools = Pools, workers = Workers} = State) ->
  case ets:lookup(Pools, Name) of
    [] ->
      {reply, {error, unknow_pool}, State};
    _ ->
      Size = ets:match(Workers, #worker{pool = Name, _ = '_'}),
      Assigned = ets:match(Workers, #worker{pool = Name, assigned = false, _='_'}),
      {reply, {ok, length(Size), length(Assigned)}, State}
  end;
handle_call({assigned, Name}, _From, #state{pools = Pools, workers = Workers} = State) ->
  case ets:lookup(Pools, Name) of
    [] ->
      {reply, {error, unknow_pool}, State};
    _ ->
      Assigned = ets:match(Workers, #worker{pool = Name, assigned = true, pid = '$1', _='_'}),
      {reply, {ok, lists:flatten(Assigned)}, State}
  end;
handle_call(pools, _From, #state{pools = Pools} = State) ->
  Keys = pools(ets:first(Pools), Pools, []),
  {reply, Keys, State};
handle_call({options, Name, Options}, _From, #state{options = CommonOptions} = State) ->
  {reply,
   pool_options(Name, Options, CommonOptions),
   State};
handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

% @hidden
pools('$end_of_table', _, Result) ->
  lists:reverse(Result);
pools(Pool, Pools, Result) ->
  pools(ets:next(Pools, Pool), Pools, [Pool|Result]).

% @hidden
handle_cast(_Msg, State) ->
  {noreply, State}.

% @hidden
handle_info({'DOWN', MRef, _, _, _}, #state{workers = Workers} = State) ->
  case ets:match(Workers, #worker{mref = MRef, pid = '$1', _ = '_'}) of
    [[Pid]|_] ->
      _ = erlang:demonitor(MRef),
      _ = ets:delete(Workers, Pid),
      {noreply, State};
    _ ->
      {noreply, State}
  end;
handle_info({clean, Name}, #state{pools = Pools, workers = Workers} = State) ->
  case ets:lookup(Pools, Name) of
    [] ->
      {noreply, State};
    [#pool{max_age = MaxAge,
           initial_size = InitialSize,
           chunk_size = ChunkSize,
           clean_interval = CleanInterval,
           supervisor = SupervisorPid}] ->
      Size = length(ets:match(Workers, #worker{pool = Name, _ = '_'})),
      AssignedSize = length(ets:match(Workers, #worker{pool = Name, assigned = true, _ = '_'})),
      if
        Size > InitialSize ->
          Epoch = epoch(),
          MatchSpec = ets:fun2ms(fun(#worker{pool = Pool,
                                             assigned = false,
                                             since = Since,
                                             pid = Pid}) when Pool == Name,
                                                              Since + MaxAge < Epoch ->
                                     Pid
                                 end),
          case ets:select(Workers, MatchSpec) of
            [] -> ok;
            Candidats ->
              MaxCandidats = if
                               AssignedSize < InitialSize ->
                                 length(Candidats) - (InitialSize - AssignedSize);
                               AssignedSize == InitialSize ->
                                 length(Candidats) - 1;
                               true ->
                                 length(Candidats)
                             end,
              case ChunkSize * (MaxCandidats div ChunkSize) of
                CandidatsSize when CandidatsSize > 0 ->
                  Remove = Candidats -- lists:nthtail(CandidatsSize, Candidats),
                  lists:foreach(fun(Pid) ->
                                    case ets:lookup(Workers, Pid) of
                                      [#worker{}] ->
                                        supervisor:terminate_child(SupervisorPid, Pid);
                                      _ ->
                                        ok
                                    end
                                end, Remove);
                _ ->
                  ok
              end
          end;
        true ->
          ok
      end,
      case ets:update_element(
             Pools, Name,
             {#pool.timer, erlang:send_after(CleanInterval, self(), {clean, Name})}) of
        true ->
          {noreply, State};
        false ->
          {noreply, State}
      end
  end;
handle_info(_Info, State) ->
  {noreply, State}.

% @hidden
terminate(_Reason, _State) ->
  ok.

% @hidden
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

add_workers(Name, #state{pools = Pools, workers = Workers}) ->
  case ets:lookup(Pools, Name) of
    [#pool{supervisor = SupervisorPid,
           args = Args,
           max_size = MaxSize,
           initial_size = InitialSize,
           chunk_size = ChunkSize}] ->
      Size = ets:match(Workers, #worker{pool = Name, _ = '_'}),
      PidsAssigned = ets:match(Workers, #worker{pool = Name, assigned = true, _='_'}),
      AddSize = if
                  length(Size) == 0 -> InitialSize;
                  length(Size) < InitialSize -> InitialSize - length(Size);
                  is_integer(MaxSize) andalso length(Size) >= MaxSize -> 0;
                  length(PidsAssigned) == length(Size) -> ChunkSize;
                  true -> 0
                end,
      if
        AddSize == 0 ->
          {ok, Size};
        true ->
          Epoch = epoch(),
          Size1 = lists:foldl(fun(_, Count) ->
                                  try
                                    case supervisor:start_child(SupervisorPid, Args) of
                                      {ok, Pid} ->
                                        MRef = erlang:monitor(process, Pid),
                                        ets:insert(Workers, #worker{pool = Name,
                                                                    pid = Pid,
                                                                    mref = MRef,
                                                                    since = Epoch,
                                                                    assigned = false}),
                                        Count + 1;
                                      {ok, Pid, _} ->
                                        MRef = erlang:monitor(process, Pid),
                                        ets:insert(Workers, #worker{pool = Name,
                                                                    pid = Pid,
                                                                    mref = MRef,
                                                                    since = Epoch,
                                                                    assigned = false}),
                                        Count + 1;
                                      {error, _} ->
                                        Count
                                    end
                                  catch
                                    _:_ ->
                                      Count
                                  end
                      end, length(Size), lists:seq(1, AddSize)),
          {ok, Size1}
      end;
    _ ->
      {error, unknow_pool}
  end.

pool_options(Pool, Options, CommonOptions) ->
  ConfigOptions = case doteki:get_env([poolgirl, pools, Pool]) of
                    undefined ->
                      #{};
                    O ->
                      maps:without([autostart, start],
                                   maps:from_list(O))
                  end,
  maps:merge(maps:merge(CommonOptions, ConfigOptions), Options).

epoch() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega * 1000000 + Sec) * 1000 + round(Micro / 1000).

