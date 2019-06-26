-module(minimonkey_encode).
-include("codes.hrl").

-export([login/1]).

-export([login_successful/0,
	 login_failure/0]).

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

publish(Data) ->
    encode_payload(?PUB, Data).

publish_successful() ->
    encode_payload(?MSG, <<"publish successful">>).

publish_failure() ->
    encode_payload(?ERR, <<"publish failure">>).

%%------------------------------------------------------------------------------
%% Private
%%------------------------------------------------------------------------------

-spec encode_payload(integer(), binary()) -> binary().
encode_payload(Code, Payload) ->
    Size = byte_size(Payload),
    <<Code:8, Size:16/little, Payload/binary>>.
