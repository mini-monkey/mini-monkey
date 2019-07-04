-module(mm_test_generators).
-include_lib("proper/include/proper.hrl").

-export([blob/0,
	 blobs/0,
	 two_unique_blobs/0]).

blob() ->
    non_empty(binary()).

blobs() ->
    non_empty(list(blob())).

two_unique_blobs() ->
    ?SUCHTHAT({Blob1, Blob2}, {blob(), blob()}, Blob1 /= Blob2).
