%%%-------------------------------------------------------------------
%% @doc regis public APIs
%% @end
%%%-------------------------------------------------------------------
-module(regis).
%%%-------------------------------------------------------------------
-export([start_link/0]).
-export([stop/0]).

-export([register/2]).
-export([unregister/1]).
-export([whereis/1]).
-export([get_names/0]).
%%%-------------------------------------------------------------------
-type name() :: atom().
-type register_response() :: {error, already_names} |
                             {error, name_taken} |
                             ok.
%%%-------------------------------------------------------------------
-spec start_link() -> {ok, pid()}.
start_link() -> regis_server:start_link().

-spec stop() -> no_return().
stop() -> regis_server:stop().

-spec register(pid(), name()) -> register_response().
register(Pid, Name) -> regis_server:register(Pid, Name).

-spec unregister(name()) -> ok.
unregister(Name) -> regis_server:unregister(Name).

-spec whereis(name()) -> pid() | undefined.
whereis(Name) -> regis_server:whereis(Name).

-spec get_names() -> list().
get_names() -> regis_server:get_names().