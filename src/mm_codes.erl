%%%-------------------------------------------------------------------
%% @doc MiniMonkey Codes
%%
%%
%%
%% @end
%%%-------------------------------------------------------------------

-module(mm_codes).
-export([code_to_modification/1,
	 code_to_access_type/1]).

-include("codes.hrl").
-include_lib("eunit/include/eunit.hrl").

-type modification() :: add | revoke.
-type access_type() :: to_admin | to_pub | to_sub.

-spec code_to_modification(integer()) -> modification().

code_to_modification(?ADD_ADMIN) -> add;
code_to_modification(?REVOKE_ADMIN) -> revoke;
code_to_modification(?ADD_PUBLISH) -> add;
code_to_modification(?REVOKE_PUBLISH) -> revoke;
code_to_modification(?ADD_SUBSCRIBE) -> add;
code_to_modification(?REVOKE_SUBSCRIBE) -> revoke.

-spec code_to_access_type(integer()) -> access_type().

code_to_access_type(?ADD_ADMIN) -> to_admin;
code_to_access_type(?REVOKE_ADMIN) -> to_admin;
code_to_access_type(?ADD_PUBLISH) -> to_pub;
code_to_access_type(?REVOKE_PUBLISH) -> to_pub;
code_to_access_type(?ADD_SUBSCRIBE) -> to_sub;
code_to_access_type(?REVOKE_SUBSCRIBE) -> to_sub.

%%------------------------------------------------------------------------------
%% Tests
%%------------------------------------------------------------------------------

code_to_modification_test_() ->
    [ ?_assert(code_to_modification(?ADD_ADMIN) =:= add)
    , ?_assert(code_to_modification(?REVOKE_ADMIN) =:= revoke)
    , ?_assert(code_to_modification(?ADD_PUBLISH) =:= add)
    , ?_assert(code_to_modification(?REVOKE_PUBLISH) =:= revoke)
    , ?_assert(code_to_modification(?ADD_SUBSCRIBE) =:= add)
    , ?_assert(code_to_modification(?REVOKE_SUBSCRIBE) =:= revoke)].

code_to_access_type_test_() ->
    [ ?_assert(code_to_access_type(?ADD_ADMIN) =:= to_admin)
    , ?_assert(code_to_access_type(?REVOKE_ADMIN) =:= to_admin)
    , ?_assert(code_to_access_type(?ADD_PUBLISH) =:= to_pub)
    , ?_assert(code_to_access_type(?REVOKE_PUBLISH) =:= to_pub)
    , ?_assert(code_to_access_type(?ADD_SUBSCRIBE) =:= to_sub)
    , ?_assert(code_to_access_type(?REVOKE_SUBSCRIBE) =:= to_sub)].
