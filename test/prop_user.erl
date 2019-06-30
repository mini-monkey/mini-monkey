-module(prop_user).
-include_lib("proper/include/proper.hrl").
-import(mm_test_common, [setup/0,
			 allow_subscribe/2,
			 allow_publish/2]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

prop_user_minimal_pub_sub_test() ->
    ?FORALL({Token1, Token2, Room, Content, Tag}, {blob(), blob(), blob(), blob(), blob()},
	    begin
		%% setup the system
		setup(),
		login:add_token(Token1),
		login:add_token(Token2),

		%% make the connections
		{ok, Sock1} = gen_tcp:connect("localhost", 1773, [binary, {active, false}]),
		{ok, Sock2} = gen_tcp:connect("localhost", 1773, [binary, {active, false}]),

		%% login both
		true andalso
		    login(Sock1, Token1) andalso
		    login(Sock2, Token2) andalso

		%% enter room
		    enter(Sock1, Room) andalso
		    enter(Sock2, Room) andalso

		%% mock permissions
		    allow_subscribe(Room, Token1) andalso
		    allow_publish(Room, Token2) andalso

		%% pub / sub
		    subscribe(Sock1, Tag) andalso
		    publish(Sock2, Content) andalso

		%% receive messaage
		    receive_content(Sock1, Tag, Content)
	    end).

%%%%%%%%%%%%%%%%%%
%%% Helpers    %%%
%%%%%%%%%%%%%%%%%%

login(Sock, Token) ->
    ok = gen_tcp:send(Sock, mm_encode:login(Token)),
    {ok, mm_encode:login_successful()} =:= gen_tcp:recv(Sock, 0).

enter(Sock, Room) ->
    ok = gen_tcp:send(Sock, mm_encode:enter(Room)),
    {ok, mm_encode:enter_successful()} =:= gen_tcp:recv(Sock, 0).

subscribe(Sock, Tag) ->
    ok = gen_tcp:send(Sock, mm_encode:subscribe(Tag)),
    {ok, mm_encode:subscribe_successful()} =:= gen_tcp:recv(Sock, 0).

publish(Sock, Tag) ->
    ok = gen_tcp:send(Sock, mm_encode:publish(Tag)),
    {ok, mm_encode:publish_successful()} =:= gen_tcp:recv(Sock, 0).

receive_content(Sock, Tag, Content) ->
    timer:sleep(10),
    Payload1 = mm_encode:enter(Tag),
    Payload2 = mm_encode:publish(Content),
    {ok, <<Payload1/binary, Payload2/binary>>} =:= gen_tcp:recv(Sock, 0).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

blob() ->
    non_empty(binary()).
