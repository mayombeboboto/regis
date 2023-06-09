-module(regis_server_tests).
-include_lib("eunit/include/eunit.hrl").

-define(SETUP(Fun), {setup, fun start/0, fun stop/1, Fun}).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
start_stop_test_() ->
    {"The server can be started, stopped and has a registered name",
     ?SETUP(fun is_registered/1)}.

register_test_() ->
    [{"A process can be registered and contacted",
      ?SETUP(fun register_contact/1)},
     {"A list of registered processes can be obtained",
      ?SETUP(fun registered_list/1)},
     {"An undefined name should return 'undefined' to crash calls",
      ?SETUP(fun noregister/1)}].

unregister_test_() ->
    [{"A process that was registered can be registered again iff it was unregistered between both calls",
      ?SETUP(fun re_un_register/1)},
     {"Unregistering never crashes",
      ?SETUP(fun unregister_nocrash/1)},
     {"A crash unregisters a process",
      ?SETUP(fun crash_unregisters/1)}].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start() ->
    {ok, ServerPid} = regis_server:start_link(),
    ServerPid.

stop(_ServerPid) ->
    regis_server:stop().

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
is_registered(ServerPid) ->
    [?_assert(erlang:is_process_alive(ServerPid)),
     ?_assertEqual(ServerPid, whereis(regis_server))].

register_contact(_ServerPid) ->
    Pid = spawn_link(fun() -> callback(regcontact) end),
    timer:sleep(15),
    Ref = make_ref(),
    WherePid = regis_server:whereis(regcontact),
    regis_server:whereis(regcontact) ! {self(), Ref, hi},
    Rec = receive
            {Ref, hi} -> true
          after 2000 -> false
          end,
    [?_assertEqual(Pid, WherePid),
     ?_assert(Rec)].

registered_list(_ServerPid) ->
    L1 = regis_server:get_names(),
    Pids = [spawn(fun() -> callback(N) end) || N <- lists:seq(1, 15)],
    timer:sleep(200),
    L2 = regis_server:get_names(),
    [exit(Pid, kill) || Pid <- Pids],
    [?_assertEqual([], L1),
     ?_assertEqual(lists:sort(lists:seq(1, 15)), lists:sort(L2))].

noregister(_ServerPid) ->
    [?_assertError(badarg, regis_server:whereis(make_ref()) ! hi),
     ?_assertEqual(undefined, regis_server:whereis(make_ref()))].

re_un_register(_ServerPid) ->
    Ref = make_ref(),
    L = [regis_server:register(self(), Ref),
         regis_server:register(self(), make_ref()),
         regis_server:unregister(Ref),
         regis_server:register(self(), make_ref())],
    [?_assertEqual([ok, {error, already_named}, ok, ok], L)].

unregister_nocrash(_ServerPid) ->
    ?_assertEqual(ok, regis_server:unregister(make_ref())).

crash_unregisters(_ServerPid) ->
    Ref = make_ref(),
    Pid = spawn(fun() -> callback(Ref) end),
    timer:sleep(150),
    Pid = regis_server:whereis(Ref),
    exit(Pid, kill),
    ?_assertEqual(undefined, regis_server:whereis(Ref)).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
callback(Name) ->
    ok = regis_server:register(self(), Name),
    receive
        {From, Ref, Msg} -> From ! {Ref, Msg}
    end.