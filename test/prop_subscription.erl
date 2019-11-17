-module(prop_subscription).
-include_lib("proper/include/proper.hrl").
-import(mm_test_tokens, [god_token/0]).
-import(mm_test_generators, [blob/0,
			     two_unique_blobs/0]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_unsubscribe_missing_test() ->
    ?FORALL({Name, DummyClient}, {blob(), blob()},
	    begin
		mm_test_common:setup(),
		{ok, Room} = mm_room_sup:create_room(Name),
		ok =:= mm_room:unsubscribe(Room, DummyClient)
	    end).
