%%%-------------------------------------------------------------------
%% @doc MiniMonkey Room
%%
%% The best analogy is a chat system. You enter a room and you can
%% either post or you can subscribe
%% @end
%%%-------------------------------------------------------------------

-module(room).
-behaviour(gen_server).

%% API
-export([start_link/1,
	 publish/2,
	 subscribe/2]).

%% Behaviour
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%%-----------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

start_link(Name) ->
    gen_server:start_link(?MODULE, Name, []).

publish(Room, Payload) ->
    gen_server:cast(Room, {publish, Payload}).

subscribe(Room, Client) ->
    gen_server:cast(Room, {subscribe, Client}).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

-record(state, {name, subscribers=[]}).

%% @hidden
init(Name) ->
    {ok, #state{name=Name}}.

%% @hidden
handle_call(What, _From, State) ->
    {reply, {error, What}, State}.

%% @hidden
handle_cast({publish, Payload}, State) ->
    {noreply, priv_publish(Payload, State)};

handle_cast({subscribe, Client}, State) ->
    {noreply, priv_subscribe(Client, State)};

handle_cast(_What, State) ->
    {noreply, State}.

%% @hidden
handle_info(_What, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% Private
%%------------------------------------------------------------------------------

priv_publish(_Payload, State) ->
    State.

priv_subscribe(_Client, State) ->
    State.

%%------------------------------------------------------------------------------
%% Tests
%%------------------------------------------------------------------------------
