-module(prop_room).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_room_minimal_test() ->
    ?FORALL({Name, Content, Tag}, {blob(), blob(), blob()},
	    begin
		room_sup:start_link(),
		{ok, User} = mock_user:start_link(),
		{ok, Room} = room_sup:create_room(Name),
		{ok, 0} = room:count_subscribers(Name),
		ok = room:subscribe(Room, User, Tag),
		ok = room:publish(Room, Content),
		timer:sleep(10),
		Result = mock_user:messages(User),
		room:unsubscribe(Room, User),
		[{published, Content, Tag}] =:= Result andalso
		    {ok, 0} =:= room:count_subscribers(Name)
	    end).

prop_room_minimal_multi_test() ->
    ?FORALL({Name, Contents, Tag}, {blob(), blobs(), blob()},
	    begin
		room_sup:start_link(),
		{ok, User} = mock_user:start_link(),
		{ok, Room} = room_sup:create_room(Name),
		{ok, 0} = room:count_subscribers(Name),
		ok = room:subscribe(Room, User, Tag),
		ok = publish_many(Room, Contents),
		timer:sleep(10),
		Result = mock_user:messages(User),
		room:unsubscribe(Room, User),
		Result =:= contents_to_result(Contents, Tag) andalso
		    {ok, 0} =:= room:count_subscribers(Name)
	    end).

publish_many(_Room, []) ->
    ok;
publish_many(Room, [Content|Rest]) ->
    ok = room:publish(Room, Content),
    publish_many(Room, Rest).

contents_to_result(Contents, Tag) ->
    lists:map(fun(Content) ->
		      {published, Content, Tag}
	      end,
	      Contents).

blob() ->
    non_empty(binary()).

blobs() ->
    non_empty(list(blob())).
