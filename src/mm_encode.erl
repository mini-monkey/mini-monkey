-module(mm_encode).
-include("codes.hrl").

-export([login/1]).
-export([login_successful/0,
	 login_failure/0]).

-export([enter/1]).
-export([enter_successful/0,
	 enter_failure/0]).
-export([room_failure/0]).

-export([publish/1]).
-export([publish_successful/0,
	 publish_failure/0]).

-export([subscribe/1]).
-export([subscribe_successful/0,
	 subscribe_failure/0]).

-export([permissions_successful/0,
	 permissions_failure/0]).

-export([allow_publish/1,
	 allow_subscribe/1]).

%%------------------------------------------------------------------------------
%% Login
%%------------------------------------------------------------------------------

-spec login(binary()) -> binary().
login(Token) ->
    encode_payload(?AUTH, Token).

-spec login_successful() -> binary().
login_successful() ->
    encode_success("login").

-spec login_failure() -> binary().
login_failure() ->
    encode_failure("login").

%%------------------------------------------------------------------------------
%% Publish
%%------------------------------------------------------------------------------

enter(Data) ->
    encode_payload(?ENTER, Data).

enter_successful() ->
    encode_success("enter").

enter_failure() ->
    encode_failure("enter").

room_failure() ->
    encode_failure("room").

%%------------------------------------------------------------------------------
%% Publish
%%------------------------------------------------------------------------------

publish(Data) ->
    encode_payload(?PUB, Data).

publish_successful() ->
    encode_success("publish").

publish_failure() ->
    encode_failure("publish").

%%------------------------------------------------------------------------------
%% Subscribe
%%------------------------------------------------------------------------------

subscribe(Data) ->
    encode_payload(?SUB, Data).

subscribe_successful() ->
    encode_success("subscribe").

subscribe_failure() ->
    encode_failure("subscribe").

%%------------------------------------------------------------------------------
%% Permissions
%%------------------------------------------------------------------------------

permissions_successful() ->
    encode_success("permissions").

permissions_failure() ->
    encode_failure("permissions").

allow_publish(Token) ->
    encode_payload(?ADD_PUBLISH, Token).

allow_subscribe(Token) ->
    encode_payload(?ADD_SUBSCRIBE, Token).

%%------------------------------------------------------------------------------
%% Private
%%------------------------------------------------------------------------------

-spec encode_success(string()) -> binary().

encode_success(Message) ->
    encode_payload(?MSG, list_to_binary(Message ++ " successful")).

-spec encode_failure(string()) -> binary().

encode_failure(Message) ->
    encode_payload(?ERR, list_to_binary(Message ++ " failure")).

-spec encode_payload(integer(), binary() | string()) -> binary().
encode_payload(Code, Payload) when is_list(Payload) ->
    encode_payload(Code, list_to_binary(Payload));

encode_payload(Code, Payload) when is_binary(Payload) ->
    Size = byte_size(Payload),
    <<Code:8, Size:16/little, Payload/binary>>.
