-module(poolgirl_tests).

-include_lib("eunit/include/eunit.hrl").

-define(CONF,
        [{poolgirl,
          [
           {size, 5},
           {chunk_size, 10},
           {max_age, 120000},
           {max_size, infinity},
           {clean_interval, 60000},
           {retry_interval, 10},
           {max_retry, 0},
           {pools, [
                    {test00, [
                              {size, 10},
                              {chunk_size, 30},
                              {max_age, 120000},
                              {max_size, 100},
                              {clean_interval, 60000}
                             ]},
                    {test01, [
                              {autostart, true},
                              {start, {sample_worker, start_link, []}},
                              {size, 5},
                              {chunk_size, 10},
                              {max_age, 120000},
                              {max_size, infinity},
                              {clean_interval, 60000},
                              {retry_interval, 100},
                              {max_retry, 2}
                             ]},
                    {test02, [
                              {autostart, false},
                              {start, {sample_worker, start_link, []}},
                              {size, 5},
                              {chunk_size, 10},
                              {max_age, 120000},
                              {max_size, infinity},
                              {clean_interval, 60000}
                             ]}
                   ]}
          ]
         }]).

poolgirl_test_() ->
  {setup,
   fun() ->
       ok = doteki:set_env_from_config(?CONF),
       application:start(poolgirl)
   end,
   fun(_) ->
       application:stop(poolgirl)
   end,
   [
    fun() ->
        ?assertEqual([test01], poolgirl:pools()),
        ?assertMatch({ok, 5}, poolgirl:add_pool(test01)),
        ?assertEqual({ok, 5}, poolgirl:add_pool(test02)),
        ?assertEqual({error, missing_mfargs}, poolgirl:add_pool(test00)),
        ?assertEqual([test01, test02], poolgirl:pools()),
        ?assertEqual(ok, poolgirl:remove_all_pools())
    end,
    fun() ->
        ?assertEqual({ok, 2},
                     poolgirl:add_pool(test0, {sample_worker, start_link, []}, #{size => 2, chunk_size => 2})),
        ?assertEqual([test0], poolgirl:pools()),
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
        ?assertEqual([test0, test1], poolgirl:pools()),
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
        ?assertEqual([test0, test1, test2], poolgirl:pools()),
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
        ?assertEqual([test0, test1, test2, test3], poolgirl:pools()),
        [begin
           ?assertMatch({ok, _}, poolgirl:checkout(test3))
         end || _ <- lists:seq(1, 100)]
    end,
    fun() ->
        ?assertMatch(7,
                     poolgirl:transaction(
                       test0,
                       fun(Worker) ->
                           gen_server:call(Worker, {add, 4, 3})
                       end)),
        ?assertMatch({error, {error, undef}},
                     poolgirl:transaction(
                       test0,
                       fun(Worker) ->
                           gen_server:invalid_function(Worker)
                       end)),
        ?assertMatch(7,
                     poolgirl:transaction(
                       test0,
                       fun(Worker) ->
                           gen_server:call(Worker, {add, 4, 3})
                       end)),
        ?assertMatch(killed,
                     poolgirl:transaction(
                       test0,
                       fun(Worker) ->
                           gen_server:call(Worker, kill)
                       end)),
        ?assertMatch(7,
                     poolgirl:transaction(
                       test0,
                       fun(Worker) ->
                           gen_server:call(Worker, {add, 4, 3})
                       end))
    end,
    fun() ->
        ?assertEqual(ok, poolgirl:remove_pool(test1)),
        ?assertEqual([test0, test2, test3], poolgirl:pools()),
        ?assertEqual(ok, poolgirl:remove_all_pools()),
        ?assertEqual([], poolgirl:pools())
    end,
    fun() ->
        ets:new(test4, [named_table, public, set, {keypos, 1}]),
        ets:insert(test4, {nb, 0}),

        ?assertEqual({ok, 1},
                     poolgirl:add_pool(test4, {limited_worker, start_link, [test4, 3]}, #{size => 1, chunk_size => 1})),
        ?assertMatch({ok, _}, poolgirl:checkout(test4)),
        ?assertMatch({ok, _}, poolgirl:checkout(test4)),
        ?assertMatch({ok, _}, poolgirl:checkout(test4)),
        ?assertMatch({error, no_available_worker}, poolgirl:checkout(test4)),
        ?assertMatch({ok, 3, 0}, poolgirl:size(test4)),

        poolgirl:remove_pool(test4),
        ets:delete(test4)
    end,
    fun() ->
        ets:new(test5, [named_table, public, set, {keypos, 1}]),
        ets:insert(test5, {nb, 0}),

        ?assertEqual({ok, 3},
                     poolgirl:add_pool(test5, {limited_worker, start_link, [test5, 3]}, #{size => 10, chunk_size => 5})),
        ?assertMatch({ok, 3, 3}, poolgirl:size(test5)),
        ?assertMatch({ok, _}, poolgirl:checkout(test5)),
        ?assertMatch({ok, _}, poolgirl:checkout(test5)),
        ?assertMatch({ok, _}, poolgirl:checkout(test5)),
        ?assertMatch({error, no_available_worker}, poolgirl:checkout(test5)),
        ?assertMatch({ok, 3, 0}, poolgirl:size(test5)),

        poolgirl:remove_pool(test5),
        ets:delete(test5)
    end,
    fun() ->
        ets:new(test6, [named_table, public, set, {keypos, 1}]),
        ets:insert(test6, {nb, 0}),

        ?assertEqual({ok, 3},
                     poolgirl:add_pool(test6, {limited_worker, start_link, [test6, 3]}, #{size => 10, chunk_size => 5, max_retry => 5, retry_interval => 200})),
        ?assertMatch({ok, 3, 3}, poolgirl:size(test6)),
        ?assertMatch({ok, _}, poolgirl:checkout(test6)),
        ?assertMatch({ok, _}, poolgirl:checkout(test6)),
        {ok, W} = poolgirl:checkout(test6),
        ?assertMatch({error, no_available_worker}, poolgirl:checkout(test6)),
        ?assertMatch({ok, 3, 0}, poolgirl:size(test6)),
        erlang:spawn(fun() ->
                         timer:sleep(300),
                         ?assertEqual(ok, poolgirl:checkin(W))
                     end),
        ?assertMatch({ok, _}, poolgirl:checkout(test6)),
        ?assertMatch({ok, 3, 0}, poolgirl:size(test6)),

        poolgirl:remove_pool(test6),
        ets:delete(test6)
    end
   ]
  }.

