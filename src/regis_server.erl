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
-record(state, { pids=gb_trees:empty(),
                 names=gb_trees:empty() }).
%%%-------------------------------------------------------------------
%%% API Functions
%%%-------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

register(Name, Pid) when is_pid(Pid) ->
    gen_server:call(?MODULE, {register, Name, Pid}).

unregister(Name) ->
    gen_server:call(?MODULE, {unregister, Name}).

whereis(Name) ->
    gen_server:call(?MODULE, {whereis, Name}).

get_names() ->
    gen_server:call(?MODULE, get_names).

%%%-------------------------------------------------------------------
%%% Callback Functions
%%%-------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

handle_call({register, Name, Pid}, _From, State=#state{ pids=Pids, names=Names }) ->
    case {gb_trees:is_defined(Pid, Pids), gb_trees:is_defined(Name, Names)} of
        {true, _Boolean} ->
            {reply, {error, already_named}, State};
        {_Boolean, true} ->
            {reply, {error, name_taken}, State};
        {false, false} ->
            Ref = erlang:monitor(process, Pid),
            NewState=State#state{ pids=gb_trees:insert(Pid, {Name,Ref}, Pids),
                                  names=gb_trees:insert(Name, {Pid,Ref}, Names) },
            {reply, ok, NewState}
    end;
handle_call({unregister, Name}, _From, State=#state{ pids=Pids, names=Names }) ->
    case gb_trees:lookup(Name, Names) of
        {value, {Pid, Ref}} ->
            erlang:demonitor(Ref, [flush]),
            NewState=#state{ pids=gb_trees:delete(Pid, Pids),
                             names=gb_trees:delete(Name, Names) },
            {reply, ok, NewState};
        none ->
            {reply, ok, State}
    end;
handle_call({whereis, Name}, _From, State=#state{ names=Names }) ->
    case gb_trees:lookup(Name, Names) of
        {value, {Pid,_Ref}} ->
            {reply, Pid, State};
        none ->
            {reply, undefined, State}
    end;
handle_call(get_names, _From, State=#state{ names=Names }) ->
    {reply, gb_trees:keys(Names), State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Event, _From, State) ->
    {noreply, State}.


handle_cast(_Event, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, Pid, _Reason}, State) ->
    {value, {Name, Ref}} = gb_trees:lookup(Pid, State#state.pids),
    NewState = State#state{ pids=gb_trees:delete(Pid, State#state.pids),
                            names=gb_trees:delete(Name, State#state.names)},
    {noreply, NewState};
handle_info(_Event, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
