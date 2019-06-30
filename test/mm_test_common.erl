-module(mm_test_common).
-import(mm_test_tokens, [god_token/0]).

-export([setup/0,
	 allow_subscribe/2,
	 allow_publish/2,
	 clear/1]).

setup() ->
    os:set_env_var("god_token", binary_to_list(god_token())),
    application:ensure_all_started(minimonkey),
    true = login:is_god_token(god_token()).

allow_subscribe(Room, Token) ->
    ok =:= mm_room:permissions(Room, god_token(), add, to_sub, Token).

allow_publish(Room, Token) ->
    ok =:= mm_room:permissions(Room, god_token(), add, to_pub, Token).

clear({ok, Payload}) ->
    clear(Payload);
clear(<<_Code:8, _Size:16, Msg/binary>>) ->
    Msg.
