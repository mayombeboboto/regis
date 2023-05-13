%%%-------------------------------------------------------------------
%% @doc regis application module
%% @end
%%%-------------------------------------------------------------------
-module(regis_app).
%%%-------------------------------------------------------------------
-behaviour(application).
%%%-------------------------------------------------------------------
-export([start/2]).
-export([stop/1]).
%%%-------------------------------------------------------------------
-spec start(normal, list()) ->{ok, pid()}.
start(_StartType, _StartArgs) ->
    regis_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
