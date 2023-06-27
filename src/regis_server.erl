%%%-------------------------------------------------------------------
%% @doc regis application server
%% @end
%%%-------------------------------------------------------------------
-module(regis_server).
%%%-------------------------------------------------------------------
-behaviour(gen_server).
%%%-------------------------------------------------------------------
-export([start_link/0]).
-export([stop/0]).
-export([register/2]).
-export([unregister/1]).
-export([whereis/1]).
-export([get_names/0]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
%%%-------------------------------------------------------------------
-include_lib("stdlib/include/ms_transform.hrl").
%%%-------------------------------------------------------------------
%%% API Functions
%%%-------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

register(Pid, Name) when is_pid(Pid) ->
    gen_server:call(?MODULE, {register, Name, Pid}).

unregister(Name) ->
    gen_server:call(?MODULE, {unregister, Name}).

whereis(Name) ->
    case ets:lookup(?MODULE, Name) of
        [{Name, Pid, _Ref}] -> Pid;
        [] -> undefined
    end.

get_names() ->
    MatchSpec = ets:fun2ms(fun({Name, _Pid, _Ref}) -> Name end),
    ets:select(?MODULE, MatchSpec).

%%%-------------------------------------------------------------------
%%% Callback Functions
%%%-------------------------------------------------------------------
init([]) ->
    ?MODULE = ets:new(?MODULE, [set, named_table, protected]),
    {ok, ?MODULE}.

handle_call({register, Name, Pid}, _From, TableID) ->
    MatchSpec = ets:fun2ms(fun({N, P, _Ref}) when N==Name; P==Pid -> {N, P} end),
    case ets:select(TableID, MatchSpec) of
        [] ->
            Ref = erlang:monitor(process, Pid),
            ets:insert(TableID, {Name, Pid, Ref}),
            {reply, ok, TableID};
        [{Name, _P}|_Tail] ->
            {reply, {error, name_taken}, TableID};
        [{_N, Pid}|_Tail] ->
            {reply, {error, already_named}, TableID}
    end;
handle_call({unregister, Name}, _From, TableID) ->
    case ets:lookup(TableID, Name) of
        [{Name, _Pid, Ref}] ->
            erlang:demonitor(Ref, [flush]),
            ets:delete(TableID, Name),
            {reply, ok, TableID};
        [] ->
            {reply, ok, TableID}
    end;
handle_call(stop, _From, TableID) ->
    ets:delete(TableID),
    {stop, normal, ok, TableID};
handle_call(_Event, _From, TableID) ->
    {noreply, TableID}.


handle_cast(_Event, TableID) ->
    {noreply, TableID}.

handle_info({'DOWN', Ref, process, _Pid, _Reason}, TableID) ->
    ets:match_delete(TableID, {'_', '_', Ref}),
    {noreply, TableID};
handle_info(_Event, State) ->
    {noreply, State}.

terminate(_Reason, _TableID) ->
    ok.

%%%-------------------------------------------------------------------
%%% Internal Functions
%%%-------------------------------------------------------------------