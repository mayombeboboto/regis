%%%-------------------------------------------------------------------
%% @doc regis top level supervisor.
%% @end
%%%-------------------------------------------------------------------
-module(regis_sup).
%%%-------------------------------------------------------------------
-behaviour(supervisor).
%%%-------------------------------------------------------------------
-export([start_link/0]).

-export([init/1]).
%%%-------------------------------------------------------------------
-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 3600},
    ChildSpecs = [#{ id => server,
                     start => {regis_server, start_link, []},
                     restart => permanent,
                     shutdown => 500,
                     type => worker,
                     modules => [regis_server] }],
    {ok, {SupFlags, ChildSpecs}}.