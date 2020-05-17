%%%-------------------------------------------------------------------
%% @doc MiniMonkey User
%%
%%
%%
%% @end
%%%-------------------------------------------------------------------

-module(mm_user).
-behaviour(gen_server).
-include("codes.hrl").
-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).
-define(LOGIN_ERROR_SPEEDBUMP_MS, 0).
-else.
-define(LOGIN_ERROR_SPEEDBUMP_MS, 500).
-endif.

%% API
-export([start_link/4]).

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

start_link(_Ref, Socket, Transport, Opts) ->
    gen_server:start_link(?MODULE, [Socket, Transport, Opts], []).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

-record(state, {socket,
		transport,
		data = <<>>,
		token = missing,
		room = missing,
		rooms = #{}  %% needed so we can unsubscribe
	       }).

%% @hidden
init([Socket, Transport, _Opts = []]) ->
    Transport:setopts(Socket, [{active, true}]),
    {ok, #state{socket=Socket, transport=Transport}}.

%% @hidden
handle_call(What, _From, State) ->
    {reply, {error, What}, State}.

%% @hidden
handle_cast(What, State) ->
    lager:warning("unhandle cast ~p", [What]),
    {noreply, State}.

%% @hidden
handle_info({tcp_closed, _Port}, State) ->
    {stop, normal, State};

handle_info({tcp, _Port, DataNew}, State0=#state{data=DataOld}) ->
    Data = <<DataOld/binary, DataNew/binary>>,
    case mm_support:correctly_formated(Data) of
	{ok, Code, Payload, Rest} ->
	    {reply, Resp, State} = handle_payload(Code, Payload, State0#state{data=Rest}),
	    send_response(Resp, State),
	    {noreply, State};
	{error, Data} ->
	    {noreply, State0#state{data=Data}}
    end;

handle_info({published, Payload, Tag}, State) ->
    lager:debug("send published message with tag ~p", [Tag]),
    send_response(mm_encode:enter(Tag), State),
    send_response(mm_encode:publish(Payload), State),
    {noreply, State};

handle_info({handshake, minimonkey, _, _, _}, State) ->
    {noreply, State};

handle_info(What, State) ->
    lager:warning("unhandle info ~p", [What]),
    {noreply, State}.

%% @hidden
terminate(_Reason, State=#state{socket=Socket, transport=Transport}) ->
    unsubscribe(State),
    Transport:close(Socket),
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% Private
%%------------------------------------------------------------------------------

send_response(Payload, #state{socket=Socket, transport=Transport}) ->
    log_response(Payload),
    Transport:send(Socket, Payload).

%% Here we handle all the different payloads.
%% Remember that our connection is stateful.

handle_payload(?AUTH, Token, State) ->
    lager:debug("auth with ~p", [Token]),
    case mm_login:attempt(Token) of
	ok ->
	    {reply, mm_encode:login_successful(), State#state{token=Token}};
	_ ->
	    timer:sleep(?LOGIN_ERROR_SPEEDBUMP_MS),
	    {reply, mm_encode:login_failure(), State}
    end;

handle_payload(_, _, State=#state{token=missing}) ->
    timer:sleep(?LOGIN_ERROR_SPEEDBUMP_MS),
    {reply, mm_encode:login_failure(), State};

handle_payload(?ADD_LOGIN, Token, State=#state{token=LoginToken}) ->
    case mm_login:is_god_token(LoginToken) of
	true ->
	    mm_login:add_token(Token),
	    {reply, mm_encode:add_login_successful(), State};
	_ ->
	    {reply, mm_encode:add_login_failure(), State}
    end;

handle_payload(?ENTER, Room, State) ->
    mm_room_sup:create_room(Room),
    {reply, mm_encode:enter_successful(), note_room(Room, State)};

handle_payload(_, _, State=#state{room=missing}) ->
    {reply, mm_encode:room_failure(), State};

handle_payload(?PUB, Data, State=#state{token=Token, room=Room}) ->
    case mm_room:publish(Room, Token, Data) of
	ok ->
	    {reply, mm_encode:publish_successful(), State};
	_ ->
	    {reply, mm_encode:publish_failure(), State}
    end;

handle_payload(?SUB, Tag, State=#state{token=Token, room=Room}) ->
    case mm_room:subscribe(Room, Token, self(), Tag) of
	ok ->
	    {reply, mm_encode:subscribe_successful(), State};
	_ ->
	    {reply, mm_encode:subscribe_failure(), State}
    end;

handle_payload(Code, Token, State) when Code >= ?ADD_ADMIN andalso
					Code =< ?REVOKE_SUBSCRIBE ->
    #state{token=MyToken, room=Room} = State,
    Mod = mm_codes:code_to_modification(Code),
    Access = mm_codes:code_to_access_type(Code),
    case mm_room:permissions(Room, MyToken, Mod, Access, Token) of
	ok ->
	    {reply, mm_encode:permissions_successful(), State};
	_ ->
	    {reply, mm_encode:permissions_failure(), State}
    end;

handle_payload(?LINK_ROOM, DstRoom, State) ->
    mm_room_sup:create_room(DstRoom),
    #state{token=Token, room=Room} = State,
    case mm_room:forward_from_to(Room, DstRoom, Token) of
	ok ->
	    {reply, mm_encode:link_successful(), State};
	_ ->
	    {reply, mm_encode:link_failure(), State}
    end.

note_room(Room, State=#state{rooms=Rooms}) ->
    State#state{room=Room, rooms=Rooms#{Room => true}}.

unsubscribe(#state{rooms=#{}}) ->
    ok;
unsubscribe(#state{rooms=Rooms}) ->
    unsubscribe(Rooms);
unsubscribe([]) ->
    ok;
unsubscribe([Room|Rooms]) ->
    room:unsubscribe(Room, self()),
    unsubscribe(Rooms).

log_response(<<16#7e, _, _, Msg/binary>> ) ->
    lager:debug("[USER] ERROR: ~p", [binary_to_list(Msg)]);

log_response(<<16#7f, _, _, Msg/binary>> ) ->
    lager:debug("[USER] DEBUG: ~p", [binary_to_list(Msg)]);

log_response(Payload) ->
    lager:debug("response: ~p", [Payload]).
