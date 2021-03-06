-module(prop_room).
-include_lib("proper/include/proper.hrl").
-import(mm_test_tokens, [god_token/0]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

prop_room_minimal_test() ->
    ?FORALL({Name, Content, Tag}, {blob(), blob(), blob()},
	    begin
		mm_test_common:setup(),

		{ok, User} = mock_user:start_link(),
		{ok, Room} = mm_room_sup:create_room(Name),
		ok =  mm_room:subscribe(Room, god_token(), User, Tag),
		ok = mm_room:publish(Room, god_token(), Content),
		timer:sleep(10),
		Result = mock_user:messages(User),
		mm_room:unsubscribe(Room, User),
		[{published, Content, Tag}] =:= Result
	    end).

prop_room_minimal_multi_test() ->
    ?FORALL({Name, Contents, Tag}, {blob(), blobs(), blob()},
	    begin
		mm_test_common:setup(),

		{ok, User} = mock_user:start_link(),
		{ok, Room} = mm_room_sup:create_room(Name),
		ok = mm_room:subscribe(Room, god_token(), User, Tag),
		ok = publish_many(Room, god_token(), Contents),
		timer:sleep(10),
		Result = mock_user:messages(User),
		mm_room:unsubscribe(Room, User),
		Result =:= contents_to_result(Contents, Tag)
	    end).

prop_room_admin() ->
    ?FORALL({Name, Token}, {blob(), blob()},
	    begin
		mm_test_common:setup(),

		{ok, Room} = mm_room_sup:create_room(Name),

		true andalso
		    ok =:= mm_room:permissions(Room, god_token(), add, to_admin, Token) andalso
		    true =:= mm_room:is_admin(Room, Token) andalso
		    ok =:= mm_room:permissions(Room, god_token(), revoke, to_admin, Token) andalso
		    false =:= mm_room:is_admin(Room, Token)
	    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

publish_many(_Room, _Token, []) ->
    ok;
publish_many(Room, Token, [Content|Rest]) ->
    ok = mm_room:publish(Room, Token, Content),
    publish_many(Room, Token, Rest).

contents_to_result(Contents, Tag) ->
    lists:map(fun(Content) ->
		      {published, Content, Tag}
	      end,
	      Contents).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

blob() ->
    non_empty(binary()).

blobs() ->
    non_empty(list(blob())).
