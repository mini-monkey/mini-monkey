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
-export([start_link/2,
	 publish/3,
	 subscribe/4,
	 unsubscribe/2,
	 count_subscribers/1]).

-export([add_admin_permission/3,
	 revoke_admin_permission/3]).
-export([add_subscriber_permission/3,
	 revoke_subscriber_permission/3]).
-export([add_publisher_permission/3,
	 revoke_publisher_permission/3]).

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

start_link(GodToken, Name) ->
    gen_server:start_link(?MODULE, [GodToken, Name], []).

publish(Room, Token, Payload) ->
    gen_server:call(safe_room(Room), {publish, Token, Payload}).

subscribe(Room, Token, Client, Tag) ->
    gen_server:call(safe_room(Room), {subscribe, Token, Client, Tag}).

unsubscribe(Room, Client) ->
    gen_server:cast(safe_room(Room), {unsubscribe, Client}).

count_subscribers(Room) ->
    gen_server:call(safe_room(Room), count_subscribers).

add_admin_permission(Room, MyToken, Token) ->
    gen_server:call(safe_room(Room), {add_admin_permission, MyToken, Token}).

revoke_admin_permission(Room, MyToken, Token) ->
    gen_server:call(safe_room(Room), {revoke_admin_permission, MyToken, Token}).

add_subscriber_permission(Room, MyToken, Token) ->
    gen_server:call(safe_room(Room), {add_subscriber_permission, MyToken, Token}).

revoke_subscriber_permission(Room, MyToken, Token) ->
    gen_server:call(safe_room(Room), {revoke_subscriber_permission, MyToken, Token}).

add_publisher_permission(Room, MyToken, Token) ->
    gen_server:call(safe_room(Room), {add_publisher_permission, MyToken, Token}).

revoke_publisher_permission(Room, MyToken, Token) ->
    gen_server:call(safe_room(Room), {revoke_publisher_permission, MyToken, Token}).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

-record(state, {
	  name,
	  god_token,
	  subs = #{},
	  admins = #{},
	  pub_perms = #{},
	  sub_perms = #{},
	  last = <<>>,
	  published=0
	 }).

%% @hidden
init([GodToken, Name]) ->
    {ok, #state{name=Name, god_token=GodToken}}.

%% @hidden
handle_call({publish, Token, Payload}, _From, State) ->
    case publisher_rights(Token, State) of
	true ->
	    {reply, ok, priv_publish(Payload, State)};
	false ->
	    {reply, error, State}
    end;

handle_call({subscribe, Token, Client, Tag}, _From, State) ->
    case subscriber_rights(Token, State) of
	true ->
	    {reply, ok, priv_subscribe(Client, Tag, State)};
	_ ->
	    {reply, error, State}
    end;

handle_call(count_subscribers, _From, State=#state{subs=Subs}) ->
    {reply, {ok, maps:size(Subs)}, State};

handle_call({add_admin_permission, MyToken, Token}, _From, State) ->
    case admin_rights(MyToken, State) of
	true ->
	    {reply, ok, priv_add_admin(Token, State)};
	_ ->
	    {reply, error, State}
    end;

handle_call({revoke_admin_permission, MyToken, Token}, _From, State) ->
    case admin_rights(MyToken, State) of
	true ->
	    {reply, ok, priv_revoke_admin(Token, State)};
	_ ->
	    {reply, error, State}
    end;

handle_call({add_subscriber_permission, MyToken, Token}, _From, State) ->
    case subscriber_rights(MyToken, State) of
	true ->
	    {reply, ok, priv_add_subscriber(Token, State)};
	_ ->
	    {reply, error, State}
    end;

handle_call({revoke_subscriber_permission, MyToken, Token}, _From, State) ->
    case subscriber_rights(MyToken, State) of
	true ->
	    {reply, ok, priv_revoke_subscriber(Token, State)};
	_ ->
	    {reply, error, State}
    end;

handle_call({add_publisher_permission, MyToken, Token}, _From, State) ->
    case publisher_rights(MyToken, State) of
	true ->
	    {reply, ok, priv_add_publisher(Token, State)};
	_ ->
	    {reply, error, State}
    end;

handle_call({revoke_publisher_permission, MyToken, Token}, _From, State) ->
    case publisher_rights(MyToken, State) of
	true ->
	    {reply, ok, priv_revoke_publisher(Token, State)};
	_ ->
	    {reply, error, State}
    end;

handle_call(What, _From, State) ->
    {reply, {error, What}, State}.

%% @hidden
handle_cast({unsubscribe, Client}, State) ->
    {noreply, priv_unsubscribe(Client, State)};

handle_cast(_What, State) ->
    {noreply, State}.

%% @hidden
handle_info(_What, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, #state{name=Name}) ->
    mm_room_sup:unname_room(Name),
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

admin_rights(Token, #state{god_token=Token}) ->
    true;
admin_rights(Token, #state{admins=Admins}) ->
    maps:is_key(Token, Admins).

publisher_rights(Token, #state{god_token=Token}) ->
    true;
publisher_rights(Token, #state{pub_perms=PubPerms}) ->
    maps:is_key(Token, PubPerms).

subscriber_rights(Token, #state{god_token=Token}) ->
    true;
subscriber_rights(Token, #state{sub_perms=SubPerms}) ->
    maps:is_key(Token, SubPerms).

priv_add_admin(Token, State=#state{admins=Admins}) ->
    State#state{admins=Admins#{Token => true}}.

priv_revoke_admin(Token, State=#state{admins=Admins}) ->
    State#state{admins=maps:remove(Token, Admins)}.

priv_add_subscriber(Token, State=#state{sub_perms=SubPerms}) ->
    State#state{sub_perms=SubPerms#{Token => true}}.

priv_revoke_subscriber(Token, State=#state{sub_perms=SubPerms}) ->
    State#state{sub_perms=maps:remove(Token, SubPerms)}.

priv_add_publisher(Token, State=#state{pub_perms=SubPerms}) ->
    State#state{pub_perms=SubPerms#{Token => true}}.

priv_revoke_publisher(Token, State=#state{pub_perms=PubPerms}) ->
    State#state{pub_perms=maps:remove(Token, PubPerms)}.

safe_room(Name) when is_binary(Name) ->
    {ok, Room} = mm_room_sup:name_to_room(Name),
    Room;
safe_room(Room) when is_pid(Room) ->
    Room;
safe_room(Room) ->
    lager:warning("safe room failed for ~p", [Room]),
    {error, safe_room}.


%%------------------------------------------------------------------------------
%% Tests
%%------------------------------------------------------------------------------
