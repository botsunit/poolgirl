-module(poolgirl_tests).

-include_lib("eunit/include/eunit.hrl").

poolgirl_test_() ->
  {setup, 
   fun() -> 
       application:start(poolgirl)
   end, 
   fun(_) ->
       application:stop(poolgirl)
   end,
   [
    fun() ->
        ?assertEqual({ok, 2},
                     poolgirl:add_pool(test, {sample_worker, start_link, []}, #{size => 2, chunk_size => 2})),
        ?assertEqual({ok, 2, 2},
                     poolgirl:size(test)),
        {ok, W1} = poolgirl:checkout(test),
        ?assertEqual({ok, 2, 1},
                     poolgirl:size(test)),
        {ok, W2} = poolgirl:checkout(test),
        ?assertEqual({ok, 4, 2},
                     poolgirl:size(test)),
        ?assertEqual(ok, 
                     poolgirl:checkin(W1)),
        ?assertEqual({ok, 4, 3},
                     poolgirl:size(test)),
        ?assertEqual(ok, 
                     poolgirl:checkin(W2)),
        ?assertEqual({ok, 4, 4},
                     poolgirl:size(test))
    end
   ]
  }.

