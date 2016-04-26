

# Erlang worker pool #

Copyright (c) 2016 BotsUnit

__Version:__ 0.0.1

__Authors:__ Gregoire Lejeune ([`gregoire.lejeune@botsunit.com`](mailto:gregoire.lejeune@botsunit.com)).


### Usage ###

```

1> application:start(poolgirl).
ok
2> poolgirl:add_pool(test, {my_client, start_link, [{127,0,0,1}, 1234]}).
{ok,5}
3> Worker = poolgirl:checkout(test).
<0.89.0>
4> gen_server:call(Worker, Request).
ok
5> poolgirl:checkin(Worker).
ok

```

where `my_client.erl` is something like :

```

-module(my_client).
-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link(Addr, Port) ->
  gen_server:start_link(?MODULE, {Addr, Port}, []).

init({Addr, Port}) ->
  _ = process_flag(trap_exit, true),
  case gen_tcp:connect(Addr, Port, [{mode, binary},
                                    {active, once}]) of
    {error, Reason} ->
      {stop, Reason};
    OK ->
      OK
  end.

handle_call({call, Request}, _From, Socket) ->
  case gen_tcp:send(Socket, Request) of
    ok ->
      case inet:setopts(Socket, [{active, once}]) of
        ok ->
          {reply, ok, Socket}
        Other1 ->
          {reply, Other1, Socket}
      end;
    Other2 ->
      {reply, Other2, State1}
  end;
handle_call(_Request, _From, Socket) ->
  {reply, ok, Socket}.

handle_cast(_Msg, Socket) ->
  {noreply, Socket}.

handle_info({tcp, _, Packet}, Socket) ->
  % Do something with Packet
  {noreply, Socket};
handle_info({tcp_closed, Socket}, Socket) ->
  {stop, disconnect, Socket};
handle_info(Info, Socket) ->
  {noreply, Socket}.

terminate(_Reason, #{socket := Socket}) ->
  _ = gen_tcp:close(Socket),
  ok.

code_change(_OldVsn, Socket, _Extra) ->
  {ok, Socket}.
```


### Contributing ###
1. Fork it ( https://github.com/botsunit/poolgirl/fork )
1. Create your feature branch (`git checkout -b my-new-feature`)
1. Commit your changes (`git commit -am 'Add some feature'`)
1. Push to the branch (`git push origin my-new-feature`)
1. Create a new Pull Request



### Licence ###

poolgirl is available for use under the following license, commonly known as the 3-clause (or "modified") BSD license:

Copyright (c) 2016 BotsUnit<br />

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:* Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
* The name of the author may not be used to endorse or promote products derived from this software without specific prior written permission.



THIS SOFTWARE IS PROVIDED BY THE AUTHOR `AS IS` AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/botsunit/poolgirl/blob/master/doc/poolgirl.md" class="module">poolgirl</a></td></tr></table>

