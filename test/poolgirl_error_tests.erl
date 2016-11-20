-module(poolgirl_error_tests).
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

poolgirl_error_tests_test_() ->
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
         ?assertException(exit, {timeout, _},
                      poolgirl:add_pool(failed, {failed_worker, start_link, []}, #{size => 1, chunk_size => 1}))
     end}
   ]}.
