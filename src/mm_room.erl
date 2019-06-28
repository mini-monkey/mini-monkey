%%%-------------------------------------------------------------------
%% @doc MiniMonkey Room
%%
%% The best analogy is a chat system. You enter a room and you can
%% either post or you can subscribe
%% @end
%%%-------------------------------------------------------------------

-module(mm_room).
-behaviour(gen_server).

%% API
-export([start_link/1,
	 publish/2,
	 subscribe/3,
	 unsubscribe/2,
	 count_subscribers/1]).

-export([add_admin_permission/3,
	 revoke_admin_permission/3]).

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

%% @doc publish
%% Publish a message to a room

-spec publish(binary() | pid(), binary()) -> 'ok' | 'error'.
publish(Name, Payload) when is_binary(Name) ->
    {ok, Room} = room_sup:name_to_room(Name),
    publish(Room, Payload);
publish(Room, Payload) when is_pid(Room) ->
    gen_server:cast(Room, {publish, Payload}).

%% @doc publish
%% Publish a message to a room

-spec subscribe(binary() | pid(), binary(), binary()) -> 'ok' | 'error'.
subscribe(Name, Client, Tag) when is_binary(Name) ->
    {ok, Room} = room_sup:name_to_room(Name),
    subscribe(Room, Client, Tag);
subscribe(Room, Client, Tag) when is_pid(Room) ->
    gen_server:cast(Room, {subscribe, Client, Tag}).

-spec unsubscribe(binary() | pid(), binary()) -> 'ok' | 'error'.
unsubscribe(Name, Client) when is_binary(Name) ->
    {ok, Room} = room_sup:name_to_room(Name),
    unsubscribe(Room, Client);
unsubscribe(Room, Client) when is_pid(Room) ->
    gen_server:cast(Room, {unsubscribe, Client}).

-spec count_subscribers(binary() | pid()) -> integer().
count_subscribers(Name) when is_binary(Name) ->
    {ok, Room} = mm_room_sup:name_to_room(Name),
    count_subscribers(Room);
count_subscribers(Room) when is_pid(Room) ->
    gen_server:call(Room, count_subscribers).

-spec add_admin_permission(binary() | pid(), binary(), binary()) -> 'ok' | 'error'.
add_admin_permission(Name, MyToken, Token) when is_binary(Name) ->
    {ok, Room} = room_sup:name_to_room(Name),
    add_admin_permission(Room, MyToken, Token);
add_admin_permission(Room, MyToken, Token) when is_pid(Room) ->
    gen_server:cast(Room, {add_admin_permission, MyToken, Token}).

-spec revoke_admin_permission(binary() | pid(), binary(), binary()) -> 'ok' | 'error'.
revoke_admin_permission(Name, MyToken, Token) when is_binary(Name) ->
    {ok, Room} = room_sup:name_to_room(Name),
    revoke_admin_permission(Room, MyToken, Token);
revoke_admin_permission(Room, MyToken, Token) when is_pid(Room) ->
    gen_server:cast(Room, {revoke_admin_permission, MyToken, Token}).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

-record(state, {
	  name,
	  subs = #{},
	  admins = #{},
	  writers = #{},
	  readers = #{},
	  last = <<>>,
	  published=0
	 }).

%% @hidden
init(Name) ->
    {ok, #state{name=Name}}.

%% @hidden
handle_call(count_subscribers, _From, State=#state{subs=Subs}) ->
    {reply, {ok, maps:size(Subs)}, State};

handle_call({add_admin_permission, MyToken, Token}, _From, State) ->
    case adim_rights(MyToken, State) of
	true ->
	    {reply, ok, priv_add_admin(Token, State)};
	_ ->
	    {reply, error, State}
    end;

handle_call({revoke_admin_permission, MyToken, Token}, _From, State) ->
    case adim_rights(MyToken, State) of
	true ->
	    {reply, ok, priv_revoke_admin(Token, State)};
	_ ->
	    {reply, error, State}
    end;

handle_call(What, _From, State) ->
    {reply, {error, What}, State}.

%% @hidden
handle_cast({publish, Payload}, State) ->
    {noreply, priv_publish(Payload, State)};

handle_cast({subscribe, Client, Tag}, State) ->
    {noreply, priv_subscribe(Client, Tag, State)};

handle_cast({unsubscribe, Client}, State) ->
    {noreply, priv_unsubscribe(Client, State)};

handle_cast(_What, State) ->
    {noreply, State}.

%% @hidden
handle_info(_What, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, #state{name=Name}) ->
    room_sup:unname_room(Name),
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% Private
%%------------------------------------------------------------------------------

priv_publish(Payload, State=#state{subs=Subs, published=Published}) ->
    priv_notify(Payload, maps:to_list(Subs)),
    State#state{last=Payload, published=Published+1}.

priv_subscribe(Client, Tag, State=#state{subs=Subs}) ->
    State#state{subs=Subs#{Client => Tag}}.

priv_unsubscribe(Client, State=#state{subs=Subs}) ->
    State#state{subs=maps:remove(Client, Subs)}.

priv_notify(_Payload, []) ->
    ok;
priv_notify(Payload, [{Client, Tag}|Rest]) ->
    Client ! {published, Payload, Tag},
    priv_notify(Payload, Rest).

adim_rights(Token, #state{admins=Admins}) ->
    maps:is_key(Token, Admins).

priv_add_admin(Token, State=#state{admins=Admins}) ->
    State#state{admins=Admins#{Token => true}}.

priv_revoke_admin(Token, State=#state{admins=Admins}) ->
    State#state{admins=maps:remove(Token, Admins)}.

%%------------------------------------------------------------------------------
%% Tests
%%------------------------------------------------------------------------------
