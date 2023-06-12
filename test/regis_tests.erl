-module(regis_tests).
-include_lib("eunit/include/eunit.hrl").

double_register_test_() ->
    {setup,
     fun start/0,              % Setup function
     fun stop/1,               % Cleanup function
     fun two_names_one_pid/1}. % Instantiator

start() ->
    {ok, Pid} = regis:start_link(),
    Pid.

stop(_Pid) ->
    regis:stop().

two_names_one_pid(Pid) ->
    ok = regis:register(Pid, quite_a_unique_name),
    Res = regis:register(Pid, my_other_name_is_more_creative),
    [?_assertEqual({error, already_named}, Res)].