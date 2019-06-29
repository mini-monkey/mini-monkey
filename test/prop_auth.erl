-module(prop_auth).
-include_lib("proper/include/proper.hrl").
-import(mm_test_tokens, [god_token/0]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_admin_with_wrong_token_test() ->
    ?FORALL({Token, Name}, {blob(), blob()},
	    begin
		mm_room_sup:start_link(god_token()),
		{ok, Room} = mm_room_sup:create_room(Name),
		true andalso
		    error =:= mm_room:permissions(Room, Token, add, to_admin, Token) andalso
		    error =:= mm_room:permissions(Room, Token, add, to_pub, Token) andalso
		    error =:= mm_room:permissions(Room, Token, add, to_sub, Token) andalso
		    error =:= mm_room:permissions(Room, Token, revoke, to_admin, Token) andalso
		    error =:= mm_room:permissions(Room, Token, revoke, to_pub, Token) andalso
		    error =:= mm_room:permissions(Room, Token, revoke, to_sub, Token)
	    end).

prop_admin_with_correct_token_test() ->
    ?FORALL({Token, Name}, {blob(), blob()},
	    begin
		mm_room_sup:start_link(god_token()),
		{ok, Room} = mm_room_sup:create_room(Name),
		ok = mm_room:permissions(Room, god_token(), add, to_admin, Token),
		true andalso
		    ok =:= mm_room:permissions(Room, Token, add, to_admin, Token) andalso
		    ok =:= mm_room:permissions(Room, Token, add, to_pub, Token) andalso
		    ok =:= mm_room:permissions(Room, Token, add, to_sub, Token) andalso
		    ok =:= mm_room:permissions(Room, Token, revoke, to_pub, Token) andalso
		    ok =:= mm_room:permissions(Room, Token, revoke, to_sub, Token) andalso
		    %% this needs to be last or we cannot admin anymore :)
		    ok =:= mm_room:permissions(Room, Token, revoke, to_admin, Token)
	    end).

prop_subscribe_with_wrong_token_test() ->
    ?FORALL({Token, Name, Tag}, {blob(), blob(), blob()},
	    begin
		mm_room_sup:start_link(god_token()),
		{ok, User} = mock_user:start_link(),
		{ok, Room} = mm_room_sup:create_room(Name),
		error =:= mm_room:subscribe(Room, Token, User, Tag)
	    end).

prop_publish_with_wrong_token_test() ->
    ?FORALL({Token, Name, Content}, {blob(), blob(), blob()},
	    begin
		mm_room_sup:start_link(god_token()),
		{ok, Room} = mm_room_sup:create_room(Name),
		error =:= mm_room:publish(Room, Token, Content)
	    end).

prop_subscribe_with_new_token_test() ->
    ?FORALL({Token, Name, Tag}, {blob(), blob(), blob()},
	    begin
		mm_room_sup:start_link(god_token()),
		{ok, User} = mock_user:start_link(),
		{ok, Room} = mm_room_sup:create_room(Name),
		ok = mm_room:permissions(Room, god_token(), add, to_sub, Token),
		ok =:= mm_room:subscribe(Room, Token, User, Tag) andalso
		    ok =:= mm_room:permissions(Room, god_token(), revoke, to_sub, Token) andalso
		    error =:= mm_room:subscribe(Room, Token, User, Tag) andalso
		    ok =:= mm_room:unsubscribe(Room, User)
	    end).

prop_publish_with_new_token_test() ->
    ?FORALL({Token, Name, Content}, {blob(), blob(), blob()},
	    begin
		mm_room_sup:start_link(god_token()),
		{ok, Room} = mm_room_sup:create_room(Name),
		ok = mm_room:permissions(Room, god_token(), add, to_pub, Token),
		ok =:= mm_room:publish(Room, Token, Content) andalso
		    ok =:= mm_room:permissions(Room, god_token(), revoke, to_pub, Token) andalso
		    error =:= mm_room:publish(Room, Token, Content)
	    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

blob() ->
    non_empty(binary()).
