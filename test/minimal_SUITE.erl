-module(minimal_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([test_failed_auth/1,
	 test_successful_auth/1]).

all() -> [test_failed_auth,
	  test_successful_auth].

test_failed_auth(_Config) ->
    application:ensure_started(minimonkey),
    {ok, Sock} = gen_tcp:connect("localhost", 1773, [binary]),

    Token = <<"guest">>,
    ok = gen_tcp:send(Sock, minimonkey_encode:login(Token)),
    {ok, minimonkey_encode:login_failure()} =:= gen_tcp:recv(Sock, 0).

test_successful_auth(_Config) ->
    application:ensure_started(minimonkey),
    {ok, Sock} = gen_tcp:connect("localhost", 1773, [binary]),

    Token = <<"guest">>,
    ok = login:add_token(Token),
    ok = gen_tcp:send(Sock, minimonkey_encode:login(Token)),
    {ok, minimonkey_encode:login_successful()} =:= gen_tcp:recv(Sock, 0).
