-module(prop_room).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_file_test() ->
    ?FORALL({Name, Content, Tag}, {blob(), blob(), blob()},
	    begin
		{ok, User} = mock_user:start_link(),
		{ok, Room} = room:start_link(Name),
		ok = room:subscribe(Room, User, Tag),
		ok = room:publish(Room, Content),
		timer:sleep(10),
		Result = mock_user:messages(User),
		[{published, Content, Tag}] =:= Result
	    end).

blob() ->
    non_empty(binary()).
