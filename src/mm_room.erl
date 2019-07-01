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
	 count_subscribers/1,
	 permissions/5,
	 is_admin/2]).

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

permissions(Room, MyToken, Modification, AccessType, Token) ->
    gen_server:call(safe_room(Room), {permissions, MyToken, Modification, AccessType, Token}).

is_admin(Room, Token) ->
    gen_server:call(safe_room(Room), {is_admin, Token}).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

-record(state, {
	  name,
	  god_token,
	  subs = #{},
	  permissions = #{
	    to_admin => #{},
	    to_pub => #{},
	    to_sub => #{}},
	  last = <<>>,
	  published=0
	 }).

%% @hidden
init([GodToken, Name]) ->
    {ok, #state{name=Name, god_token=GodToken}}.

%% @hidden
handle_call({publish, Token, Payload}, _From, State) ->
    case rights(to_pub, Token, State) of
	true ->
	    {reply, ok, priv_publish(Payload, State)};
	false ->
	    {reply, error, State}
    end;

handle_call({subscribe, Token, Client, Tag}, _From, State) ->
    case rights(to_sub, Token, State) of
	true ->
	    {reply, ok, priv_subscribe(Client, Tag, State)};
	_ ->
	    {reply, error, State}
    end;

handle_call(count_subscribers, _From, State=#state{subs=Subs}) ->
    {reply, {ok, maps:size(Subs)}, State};

handle_call({permissions, MyToken, Modification, AccessType, Token}, _From, State) ->
    case rights(to_admin, MyToken, State) of
	true ->
	    {reply, ok, priv_permissions(Modification, AccessType, Token, State)};
	_ ->
	    {reply, error, State}
    end;

handle_call({is_admin, Token}, _From, State) ->
    {reply, rights(to_admin, Token, State), State};

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

priv_permissions(add, AccessType, Token, State) ->
    priv_add_permissions(AccessType, Token, State);
priv_permissions(revoke, AccessType, Token, State) ->
    priv_revoke_permissions(AccessType, Token, State).

priv_add_permissions(AccessType, Token, State=#state{permissions=Permissions}) ->
    #{AccessType := Previous} = Permissions,
    State#state{permissions=Permissions#{AccessType => Previous#{Token => true}}}.

priv_revoke_permissions(AccessType, Token, State=#state{permissions=Permissions}) ->
    #{AccessType := Previous} = Permissions,
    State#state{permissions=Permissions#{AccessType => maps:remove(Token, Previous)}}.

rights(_, Token, #state{god_token=Token}) ->
    true;
rights(AccessType, Token, #state{permissions=Permissions}) ->
    #{AccessType := Current} = Permissions,
    maps:is_key(Token, Current).

safe_room(Name) when is_binary(Name) ->
    {ok, Room} = mm_room_sup:name_to_room(Name),
    Room;
safe_room(Room) when is_pid(Room) ->
    Room.

%%------------------------------------------------------------------------------
%% Tests
%%------------------------------------------------------------------------------
