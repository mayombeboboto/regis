%%%-------------------------------------------------------------------
%% @doc regis public APIs
%% @end
%%%-------------------------------------------------------------------
-module(regis).
%%%-------------------------------------------------------------------
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
-spec register(name(), pid()) -> register_response().
register(Name, Pid) -> regis_server:register(Name, Pid).

-spec unregister(name()) -> ok.
unregister(Name) -> regis_server:unregister(Name).

-spec whereis(name()) -> pid() | undefined.
whereis(Name) -> regis_server:whereis(Name).

-spec get_names() -> list().
get_names() -> regis_server:get_names().