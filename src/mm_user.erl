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
-define(LOGIN_ERROR_SPEEDBUMP_MS, 500).

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

start_link(Ref, Socket, Transport, Opts) ->
    gen_server:start_link(?MODULE, [Socket, Transport, Opts], []).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

-record(state, {socket,
		transport,
		data = <<>>,
		token = missing,
		room = <<>>}).

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
handle_info({tcp_close, _Port}, State) ->
    {stop, normal, State};

handle_info({tcp, _Port, DataNew}, State0=#state{data=DataOld}) ->
    lager:warning("got new data: ~p", [DataNew]),
    Data = <<DataOld/binary, DataNew/binary>>,
    lager:warning("total data  : ~p", [Data]),

    case correct_formated(Data) of
	{ok, Code, Payload, Rest} ->
	    case handle_payload(Code, Payload, State0#state{data=Rest}) of
		{reply, Resp, State} ->
		    send_response(Resp, State),
		    {noreply, State};
		_ ->
		    {noreply, State0}
	    end;
	{error, Data} ->
	    {noreply, State0#state{data=Data}}
    end;

handle_info({published, Payload, Tag}, State) ->
    lager:warning("send published message with tag ~p", [Tag]),
    send_response(mm_encode:enter(Tag), State),
    send_response(mm_encode:publish(Payload), State),
    {noreply, State};

handle_info(What, State) ->
    lager:warning("unhandle info ~p", [What]),
    {noreply, State}.

%% @hidden
terminate(Reason, _State) ->
    lager:warning("terminate ~p", [Reason]),
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% Private
%%------------------------------------------------------------------------------

send_response(Payload, #state{socket=Socket, transport=Transport}) ->
    lager:warning("response: ~p", [Payload]),
    Transport:send(Socket, Payload).

%% Here we handle all the different payloads.
%% Remember that our connection is stateful.

handle_payload(?AUTH, Token, State) ->
    lager:warning("auth with ~p", [Token]),
    case login:attempt(Token) of
	ok ->
	    {reply, mm_encode:msg("logged in"), State#state{token=Token}};
	_ ->
	    timer:sleep(?LOGIN_ERROR_SPEEDBUMP_MS),
	    {reply, mm_encode:err("login failed"), State}
    end;

handle_payload(_, _, State=#state{token=missing}) ->
    {reply, mm_encode:err("please login"), State};

handle_payload(?ENTER, Room, State) ->
    room_sup:create_room(Room),
    {reply, mm_encode:msg("ok"), State#state{room=Room}};

handle_payload(?PUB, Data, State=#state{token=Token, room=Room}) ->
    case login:may_token_publish_in_room(Token, Room) of
	ok ->
	    room:publish(Room, Data),
	    {reply, mm_encode:msg("ok"), State};
	_ ->
	    {reply, mm_encode:err("authorization failed"), State}
    end;

handle_payload(?SUB, Tag, State=#state{token=Token, room=Room}) ->
    case login:may_token_subscribe_in_room(Token, Room) of
	ok ->
	    room:subscribe(Room, self(), Tag),
	    {reply, mm_encode:msg("ok"), State};
	_ ->
	    {reply, mm_encode:err("authorization failed"), State}
    end.

correct_formated(<<Code:8, Size:16/little, Data/binary>>) when Size < byte_size(Data) ->
    <<Payload:Size/binary, Rest/binary>> = Data,
    {ok, Code, Payload, <<>>};

correct_formated(<<Code:8, Size:16/little, Payload/binary>>) when Size == byte_size(Payload) ->
    {ok, Code, Payload, <<>>};

correct_formated(Data) ->
    {error, Data}.

%%------------------------------------------------------------------------------
%% Tests
%%------------------------------------------------------------------------------
