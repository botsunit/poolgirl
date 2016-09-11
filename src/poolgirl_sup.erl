% @hidden
-module(poolgirl_sup).
-behaviour(supervisor).

-export([
         start_link/0,
         add_pool/2,
         remove_pool/1
        ]).
-export([init/1]).

start_link() ->
  autostart_pools(supervisor:start_link({local, ?MODULE}, ?MODULE, [])).

autostart_pools(Result) ->
  [poolgirl:add_pool(Pool) ||
   {Pool, _} <- doteki:get_env([poolgirl, pools], []), autostart(Pool)],
  Result.

autostart({_, Options}) ->
  case lists:keyfind(autostart, 1, Options) of
    {autostart, true} ->
      case lists:keyfind(start, 1, Options) of
        {start, {_, _, _}} ->
          true;
        _ ->
          false
      end;
    _ ->
      false
  end.

add_pool(Name, {Module, Function, _}) ->
  case supervisor:start_child(
         ?MODULE,
         #{id => Name,
           start => {poolgirl_worker_sup, start_link, [Name, Module, Function]},
           restart => permanent,
           shutdown => 5000,
           type => supervisor,
           modules => [poolgirl_worker_sup]}) of
    {ok, PID} ->
      {ok, PID};
    {ok, PID, _Info} ->
      {ok, PID};
    {error, {already_started, PID}} ->
      {ok, PID};
    E ->
      E
  end.

remove_pool(PID) ->
  case supervisor:terminate_child(?MODULE, PID) of
    ok ->
      supervisor:delete_child(?MODULE, PID);
    E -> E
  end.

init([]) ->
  {ok, {
     #{strategy => one_for_all,
       intensity => 5,
       period => 10},
     [#{id => poolgirl,
        start => {poolgirl, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [poolgirl]}]
    }}.
