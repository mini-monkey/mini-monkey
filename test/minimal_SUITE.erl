-module(minimal_SUITE).
-include_lib("common_test/include/ct.hrl").
-import(mm_test_tokens, [god_token/0]).
-import(mm_test_common, [setup/0]).

-export([all/0]).
-export([test_failed_auth/1,
	 test_god_token_auth/1,
	 test_successful_auth/1]).

all() -> [test_failed_auth,
	  test_god_token_auth,
	  test_successful_auth].

test_failed_auth(_Config) ->
    setup(),
    {ok, Sock} = gen_tcp:connect("localhost", 1773, [binary]),

    Token = <<"guest">>,
    ok = gen_tcp:send(Sock, mm_encode:login(Token)),
    {ok, mm_encode:login_failure()} =:= gen_tcp:recv(Sock, 0).

test_god_token_auth(_Config) ->
    setup(),
    {ok, Sock} = gen_tcp:connect("localhost", 1773, [binary]),

    ok = gen_tcp:send(Sock, mm_encode:login(god_token())),
    {ok, mm_encode:login_successful()} =:= gen_tcp:recv(Sock, 0).

test_successful_auth(_Config) ->
    setup(),
    {ok, Sock} = gen_tcp:connect("localhost", 1773, [binary]),

    Token = <<"guest">>,
    ok = mm_login:add_token(Token),
    ok = gen_tcp:send(Sock, mm_encode:login(Token)),
    {ok, mm_encode:login_successful()} =:= gen_tcp:recv(Sock, 0).
