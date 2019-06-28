-module(prop_room).
-include_lib("proper/include/proper.hrl").
-define(GOD_TOKEN, <<"rNtiPfxtfRBXrb4e9wh8qnTSir83vTeU4WHj7pwq">>).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

prop_room_minimal_test() ->
    ?FORALL({Name, Content, Tag}, {blob(), blob(), blob()},
	    begin
		mm_room_sup:start_link(?GOD_TOKEN),
		{ok, User} = mock_user:start_link(),
		{ok, Room} = mm_room_sup:create_room(Name),
		{ok, 0} = mm_room:count_subscribers(Name),
		ok =  mm_room:subscribe(Room, ?GOD_TOKEN, User, Tag),
		ok = mm_room:publish(Room, ?GOD_TOKEN, Content),
		timer:sleep(10),
		Result = mock_user:messages(User),
		mm_room:unsubscribe(Room, User),
		[{published, Content, Tag}] =:= Result andalso
		    {ok, 0} =:= mm_room:count_subscribers(Name)
	    end).

prop_room_minimal_multi_test() ->
    ?FORALL({Name, Contents, Tag}, {blob(), blobs(), blob()},
	    begin
		mm_room_sup:start_link(?GOD_TOKEN),
		{ok, User} = mock_user:start_link(),
		{ok, Room} = mm_room_sup:create_room(Name),
		{ok, 0} = mm_room:count_subscribers(Name),
		ok = mm_room:subscribe(Room, ?GOD_TOKEN, User, Tag),
		ok = publish_many(Room, ?GOD_TOKEN, Contents),
		timer:sleep(10),
		Result = mock_user:messages(User),
		mm_room:unsubscribe(Room, User),
		Result =:= contents_to_result(Contents, Tag) andalso
		    {ok, 0} =:= mm_room:count_subscribers(Name)
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
