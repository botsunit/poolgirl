-module(failed_timeout_worker).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

start_link() ->
  gen_server:start_link(?MODULE, [], [{timeout, 1000}]).

init([]) ->
  timer:sleep(5000),
  {ok, worker}.

handle_call(kill, _, State) ->
  {stop, {error, killed}, killed, State};
handle_call({add, A, B}, _, State) ->
  {reply, A + B, State};
handle_call(_, _, State) ->
  {reply, ok, State}.

handle_cast(_, State) ->
  {noreply, State}.

handle_info(_, State) ->
  {noreply, State}.

terminate(_, _) ->
  ok.

code_change(_, State, _) ->
  {ok, State}.

