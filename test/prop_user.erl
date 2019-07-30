-module(prop_user).
-include_lib("proper/include/proper.hrl").
-import(mm_test_common, [setup/0,
			 allow_admin/2,
			 allow_subscribe/2,
			 allow_publish/2,
			 disallow_admin/2,
			 disallow_subscribe/2,
			 disallow_publish/2]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

prop_user_minimal_pub_sub_test() ->
    ?FORALL({Token1, Token2, Room, Content, Tag}, {blob(), blob(), blob(), blob(), blob()},
	    begin
		%% setup the system
		setup(),
		mm_login:add_token(Token1),
		mm_login:add_token(Token2),

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
		    receive_content(Sock1, Tag, Content) andalso

		%% revoke permissions
		    disallow_subscribe(Room, Token1) andalso
		    disallow_publish(Room, Token2) andalso

		%% make sure we cannot subscribe and publish
		    cannot_subscribe(Sock1, Tag) andalso
		    cannot_publish(Sock2, Content) andalso

		%% clean-up
		    close(Sock1) andalso
		    close(Sock2)
	    end).

prop_admin_for_pub() ->
    ?FORALL({TokenAdmin, TokenPub, Room, Content}, {blob(), blob(), blob(), blob()},
	    begin
		setup(),
		mm_login:add_token(TokenAdmin),
		mm_login:add_token(TokenPub),

		%% make the connections
		{ok, Sock1} = gen_tcp:connect("localhost", 1773, [binary, {active, false}]),
		{ok, Sock2} = gen_tcp:connect("localhost", 1773, [binary, {active, false}]),

		%% login both
		true andalso
		    login(Sock1, TokenAdmin) andalso
		    login(Sock2, TokenPub) andalso

		%% enter room
		    enter(Sock1, Room) andalso
		    enter(Sock2, Room) andalso

		%% allow admin to admin (using god_token)
		    allow_admin(Room, TokenAdmin) andalso

		%% publisher cannot publish without permission
		    cannot_publish(Sock2, Content) andalso

		%% allow publisher to publish using admin token
		    admin_publish(Sock1, TokenPub) andalso

		%% publish a message
		    publish(Sock2, Content) andalso

		%% clean-up
		    close(Sock1) andalso
		    close(Sock2)
	    end).

prop_admin_for_sub() ->
    ?FORALL({TokenAdmin, TokenSub, Room, Tag}, {blob(), blob(), blob(), blob()},
	    begin
		setup(),
		mm_login:add_token(TokenAdmin),
		mm_login:add_token(TokenSub),

		%% make the connections
		{ok, Sock1} = gen_tcp:connect("localhost", 1773, [binary, {active, false}]),
		{ok, Sock2} = gen_tcp:connect("localhost", 1773, [binary, {active, false}]),

		%% login both
		true andalso
		    login(Sock1, TokenAdmin) andalso
		    login(Sock2, TokenSub) andalso

		%% enter room
		    enter(Sock1, Room) andalso
		    enter(Sock2, Room) andalso

		%% allow admin to admin (using god_token)
		    allow_admin(Room, TokenAdmin) andalso

		%% publisher cannot publish without permission
		    cannot_subscribe(Sock2, Tag) andalso

		%% allow publisher to publish using admin token
		    admin_subscribe(Sock1, TokenSub) andalso

		%% publish a message
		    subscribe(Sock2, Tag) andalso

		%% clean-up
		    close(Sock1) andalso
		    close(Sock2)
	    end).

prop_enter_without_login() ->
    ?FORALL({Token, Room}, {blob(), blob()},
	    begin
		mm_test_common:setup(),
		mm_login:add_token(Token),

		%% make the connections
		{ok, Sock} = gen_tcp:connect("localhost", 1773, [binary, {active, false}]),

		%% attempt to enter without logging in
		cannot_enter(Sock, Token) andalso

		%% login and succeed to enter
		    login(Sock, Token) andalso
		    enter(Sock, Room)
	    end).

prop_pub_without_enter() ->
    ?FORALL({Token, Content}, {blob(), blob()},
	    begin
		mm_test_common:setup(),
		mm_login:add_token(Token),

		%% make the connections and login
		{ok, Sock} = gen_tcp:connect("localhost", 1773, [binary, {active, false}]),
		login(Sock, Token) andalso

		%% attempt to enter without logging in
		    room_error_for_publish(Sock, Content)
	    end).


%%%%%%%%%%%%%%%%%%
%%% Helpers    %%%
%%%%%%%%%%%%%%%%%%

close(Sock) ->
    ok =:= gen_tcp:close(Sock).

login(Sock, Token) ->
    ok = gen_tcp:send(Sock, mm_encode:login(Token)),
    {ok, mm_encode:login_successful()} =:= gen_tcp:recv(Sock, 0).

enter(Sock, Room) ->
    ok = gen_tcp:send(Sock, mm_encode:enter(Room)),
    {ok, mm_encode:enter_successful()} =:= gen_tcp:recv(Sock, 0).

cannot_enter(Sock, Room) ->
    ok = gen_tcp:send(Sock, mm_encode:enter(Room)),
    {ok, mm_encode:login_failure()} =:= gen_tcp:recv(Sock, 0).

subscribe(Sock, Tag) ->
    ok = gen_tcp:send(Sock, mm_encode:subscribe(Tag)),
    {ok, mm_encode:subscribe_successful()} =:= gen_tcp:recv(Sock, 0).

cannot_subscribe(Sock, Tag) ->
    ok = gen_tcp:send(Sock, mm_encode:subscribe(Tag)),
    {ok, mm_encode:subscribe_failure()} =:= gen_tcp:recv(Sock, 0).

publish(Sock, Content) ->
    ok = gen_tcp:send(Sock, mm_encode:publish(Content)),
    {ok, mm_encode:publish_successful()} =:= gen_tcp:recv(Sock, 0).

cannot_publish(Sock, Content) ->
    ok = gen_tcp:send(Sock, mm_encode:publish(Content)),
    {ok, mm_encode:publish_failure()} =:= gen_tcp:recv(Sock, 0).

receive_content(Sock, Tag, Content) ->
    timer:sleep(10),
    Payload1 = mm_encode:enter(Tag),
    Payload2 = mm_encode:publish(Content),
    {ok, <<Payload1/binary, Payload2/binary>>} =:= gen_tcp:recv(Sock, 0).

admin_publish(Sock, Token) ->
    ok = gen_tcp:send(Sock, mm_encode:allow_publish(Token)),
    {ok, mm_encode:permissions_successful()} =:= gen_tcp:recv(Sock, 0).

admin_subscribe(Sock, Token) ->
    ok = gen_tcp:send(Sock, mm_encode:allow_subscribe(Token)),
    {ok, mm_encode:permissions_successful()} =:= gen_tcp:recv(Sock, 0).

room_error_for_publish(Sock, Content) ->
    ok = gen_tcp:send(Sock, mm_encode:publish(Content)),
    {ok, mm_encode:room_failure()} =:= gen_tcp:recv(Sock, 0).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

blob() ->
    non_empty(binary()).
