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
-record(state, { pid=gb_trees:empty(),
                 name=gb_trees:empty() }).
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

handle_call({register, Name, Pid}, _From, State=#state{ pid=P, name=N }) ->
    case {gb_trees:is_defined(Pid, P), gb_trees:is_defined(Name, N)} of
        {true, _Boolean} ->
            {reply, {error, already_named}, State};
        {_Boolean, true} ->
            {reply, {error, name_taken}, State};
        {false, false} ->
            Ref = erlang:monitor(process, Pid),
            NewState=State#state{ pid=gb_trees:insert(Pid, {Name,Ref}, P),
                                  name=gb_trees:insert(Name, {Pid,Ref}, N) },
            {reply, ok, NewState}
    end;
handle_call({unregister, Name}, _From, State=#state{ pid=P, name=N }) ->
    case gb_trees:lookup(Name, N) of
        {value, {Pid,Ref}} ->
            erlang:demonitor(Ref, [flush]),
            NewState=#state{ pid=gb_trees:delete(Pid, P),
                             name=gb_trees:delete(Name, N) },
            {reply, ok, NewState};
        none ->
            {reply, ok, State}
    end;
handle_call({whereis, Name}, _From, State=#state{ name=N }) ->
    case gb_trees:lookup(Name, N) of
        {value, {Pid,_}} ->
            {reply, Pid, State};
        none ->
            {reply, undefined, State}
    end;
handle_call(get_names, _From, State=#state{ name=N }) ->
    {reply, gb_trees:keys(N), State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Event, _From, State) ->
    {noreply, State}.


handle_cast(_Event, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, Pid, _Reason}, State) ->
    {value, {Name, Ref}} = gb_trees:lookup(Pid, State#state.pid),
    NewState = State#state{ pid=gb_trees:delete(Pid, State#state.pid),
                            name=gb_trees:delete(Name, State#state.name)},
    {noreply, NewState};
handle_info(_Event, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
