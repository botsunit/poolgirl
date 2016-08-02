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
                     poolgirl:add_pool(test0, {sample_worker, start_link, []}, #{size => 2, chunk_size => 2})),
        ?assertEqual({ok, 2, 2},
                     poolgirl:size(test0)),
        {ok, W1} = poolgirl:checkout(test0),
        ?assertEqual({ok, 2, 1},
                     poolgirl:size(test0)),
        {ok, W2} = poolgirl:checkout(test0),
        ?assertEqual({ok, 4, 2},
                     poolgirl:size(test0)),
        ?assertEqual(ok,
                     poolgirl:checkin(W1)),
        ?assertEqual({ok, 4, 3},
                     poolgirl:size(test0)),
        ?assertEqual(ok,
                     poolgirl:checkin(W2)),
        ?assertEqual({ok, 4, 4},
                     poolgirl:size(test0))
    end,
    fun() ->
        ?assertEqual({ok, 1},
                     poolgirl:add_pool(test1, {sample_worker, start_link, []}, #{size => 1, chunk_size => 1, max_size => 3})),
        ?assertMatch({ok, _}, poolgirl:checkout(test1)),
        ?assertMatch({ok, _}, poolgirl:checkout(test1)),
        {ok, W2} = poolgirl:checkout(test1),
        ?assertEqual({error, no_available_worker}, poolgirl:checkout(test1)),
        ?assertEqual(ok, poolgirl:checkin(W2)),
        ?assertMatch({ok, _}, poolgirl:checkout(test1)),
        ?assertEqual({error, no_available_worker}, poolgirl:checkout(test1))
    end,
    fun() ->
        ?assertEqual({ok, 1},
                     poolgirl:add_pool(test2, {sample_worker, start_link, []}, #{size => 1, chunk_size => 1, max_size => 0})),
        ?assertMatch({ok, _}, poolgirl:checkout(test2)),
        {ok, W2} = poolgirl:checkout(test2),
        ?assertEqual({error, no_available_worker}, poolgirl:checkout(test2)),
        ?assertEqual(ok, poolgirl:checkin(W2)),
        ?assertMatch({ok, _}, poolgirl:checkout(test2)),
        ?assertEqual({error, no_available_worker}, poolgirl:checkout(test2))
    end,
    fun() ->
        ?assertEqual({ok, 1},
                     poolgirl:add_pool(test3, {sample_worker, start_link, []}, #{size => 1, chunk_size => 1})),
        [begin
           ?assertMatch({ok, _}, poolgirl:checkout(test3))
         end || _ <- lists:seq(1, 100)]
    end
   ]
  }.

