-module(mm_test_common).
-import(mm_test_tokens, [god_token/0]).

-export([setup/0,
	 allow_admin/2,
	 allow_subscribe/2,
	 allow_publish/2,
	 disallow_admin/2,
	 disallow_subscribe/2,
	 disallow_publish/2,
	 clear/1]).

setup() ->
    os:putenv("god_token", binary_to_list(god_token())),
    application:ensure_all_started(minimonkey),
    true = mm_login:is_god_token(god_token()).

allow_admin(Room, Token) ->
    ok =:= mm_room:permissions(Room, god_token(), add, to_admin, Token).

allow_subscribe(Room, Token) ->
    ok =:= mm_room:permissions(Room, god_token(), add, to_sub, Token).

allow_publish(Room, Token) ->
    ok =:= mm_room:permissions(Room, god_token(), add, to_pub, Token).

disallow_admin(Room, Token) ->
    ok =:= mm_room:permissions(Room, god_token(), revoke, to_admin, Token).

disallow_subscribe(Room, Token) ->
    ok =:= mm_room:permissions(Room, god_token(), revoke, to_sub, Token).

disallow_publish(Room, Token) ->
    ok =:= mm_room:permissions(Room, god_token(), revoke, to_pub, Token).

clear({ok, Payload}) ->
    clear(Payload);
clear(<<_Code:8, _Size:16, Msg/binary>>) ->
    Msg.
