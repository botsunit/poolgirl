-module(poolgirl_error_with_timeout_allowed_tests).
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
           {max_retry, 0}
          ]
         }]).

poolgirl_error_with_timeout_allowed_tests_test_() ->
  {setup,
   fun() ->
       ok = doteki:set_env_from_config(?CONF),
       application:start(poolgirl)
   end,
   fun(_) ->
       application:stop(poolgirl)
   end,
   [
    {timeout, 120,
     fun() ->
         ?assertEqual([], poolgirl:pools()),
         ?assertEqual({ok, 0},
                      poolgirl:add_pool(failed,
                                        {failed_timeout_worker, start_link, []},
                                        #{size => 1, chunk_size => 1, allow_empty_pool => true})),
        ?assertEqual([failed], poolgirl:pools()),
        ?assertEqual({error, no_available_worker}, poolgirl:checkout(failed)),
        ?assertEqual(ok, poolgirl:remove_pool(failed))
     end}
   ]}.
