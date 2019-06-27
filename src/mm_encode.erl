-module(mm_encode).
-include("codes.hrl").

-export([err/1,
	 msg/1]).

-export([login/1]).
-export([login_successful/0,
	 login_failure/0]).

-export([enter/1]).
-export([enter_successful/0,
	 enter_failure/0]).


-export([publish/1]).
-export([publish_successful/0,
	 publish_failure/0]).

%%------------------------------------------------------------------------------
%% General Message
%%------------------------------------------------------------------------------

err(Msg) ->
    encode_payload(?ERR, Msg).

msg(Msg) ->
    encode_payload(?MSG, Msg).

%%------------------------------------------------------------------------------
%% Login
%%------------------------------------------------------------------------------

-spec login(binary()) -> binary().
login(Token) ->
    encode_payload(?AUTH, Token).

-spec login_successful() -> binary().
login_successful() ->
    encode_payload(?MSG, <<"login successful">>).

-spec login_failure() -> binary().
login_failure() ->
    encode_payload(?ERR, <<"login failure">>).

%%------------------------------------------------------------------------------
%% Publish
%%------------------------------------------------------------------------------

enter(Data) ->
    encode_payload(?ENTER, Data).

enter_successful() ->
    encode_payload(?MSG, <<"enter successful">>).

enter_failure() ->
    encode_payload(?ERR, <<"enter failure">>).

%%------------------------------------------------------------------------------
%% Publish
%%------------------------------------------------------------------------------

publish(Data) ->
    encode_payload(?PUB, Data).

publish_successful() ->
    encode_payload(?MSG, <<"publish successful">>).

publish_failure() ->
    encode_payload(?ERR, <<"publish failure">>).

%%------------------------------------------------------------------------------
%% Private
%%------------------------------------------------------------------------------

-spec encode_payload(integer(), binary() | string()) -> binary().

encode_payload(Code, Payload) when is_list(Payload) ->
    encode_payload(Code, list_to_binary(Payload));

encode_payload(Code, Payload) when is_binary(Payload) ->
    Size = byte_size(Payload),
    <<Code:8, Size:16/little, Payload/binary>>.
