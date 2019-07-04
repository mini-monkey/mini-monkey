-module(prop_forwards).
-include_lib("proper/include/proper.hrl").
-import(mm_test_tokens, [god_token/0]).
-import(mm_test_generators, [blob/0,
			     two_unique_blobs/0]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

prop_setup_forwards_source_problem() ->
    ?FORALL({{Src, Dst}, Token}, {two_unique_blobs(), blob()},
	    begin
		mm_test_common:setup(),

		{ok, SrcRoom} = mm_room_sup:create_room(Src),
		{ok, DstRoom} = mm_room_sup:create_room(Dst),

		error =:= mm_room:forward_from_to(SrcRoom, DstRoom, Token)
	    end).

prop_setup_forwards_destination_problem() ->
    ?FORALL({{Src, Dst}, Token}, {two_unique_blobs(), blob()},
	    begin
		mm_test_common:setup(),

		{ok, SrcRoom} = mm_room_sup:create_room(Src),
		{ok, DstRoom} = mm_room_sup:create_room(Dst),

		ok = mm_room:permissions(SrcRoom, god_token(), add, to_admin, Token),

		error =:= mm_room:forward_from_to(SrcRoom, DstRoom, Token)
	    end).

prop_setup_forwards_success() ->
    ?FORALL({{Src, Dst}, Token}, {two_unique_blobs(), blob()},
	    begin
		mm_test_common:setup(),

		{ok, SrcRoom} = mm_room_sup:create_room(Src),
		{ok, DstRoom} = mm_room_sup:create_room(Dst),

		%% we need to reset the rooms because we will
		%% be counting the forwards
		mm_room:reset(SrcRoom),
		mm_room:reset(DstRoom),

		ok = mm_room:permissions(SrcRoom, god_token(), add, to_admin, Token),
		ok = mm_room:permissions(DstRoom, god_token(), add, to_admin, Token),

		true andalso
		    ok =:= mm_room:forward_from_to(SrcRoom, DstRoom, Token) andalso
		    %% 1 =:= mm_room:count_forwards(SrcRoom) andalso
		    %% 0 =:= mm_room:count_forwards(DstRoom)
		    true
	    end).

prop_forward_minimal_test() ->
    ?FORALL({{Src, Dst}, Content, Tag}, {two_unique_blobs(), blob(), blob()},
	    begin
		mm_test_common:setup(),

		{ok, SrcRoom} = mm_room_sup:create_room(Src),
		{ok, DstRoom} = mm_room_sup:create_room(Dst),

		ok = mm_room:forward_from_to(SrcRoom, DstRoom, god_token()),

		{ok, User} = mock_user:start_link(),
		ok =  mm_room:subscribe(DstRoom, god_token(), User, Tag),

		ok = mm_room:publish(SrcRoom, god_token(), Content),
		timer:sleep(10),

		Result = mock_user:messages(User),
		mm_room:unsubscribe(DstRoom, User),
		[{published, Content, Tag}] =:= Result
	    end).
