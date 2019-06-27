-module(mm_support).

-include("codes.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([binary_env_var/2,
	 correctly_formated/1]).

binary_env_var(Name, Default) when is_list(Default) ->
    binary_env_var(Name, list_to_binary(Default));

binary_env_var(Name, Default) ->
    case os:getenv(Name) of
	false ->
	    Default;
	Var ->
	    list_to_binary(Var)
    end.

correctly_formated(<<Code:8, Size:16/little, Data/binary>>) when Size < byte_size(Data) ->
    <<Payload:Size/binary, Rest/binary>> = Data,
    {ok, Code, Payload, Rest};

correctly_formated(<<Code:8, Size:16/little, Payload/binary>>) when Size == byte_size(Payload) ->
    {ok, Code, Payload, <<>>};

correctly_formated(Data) ->
    {error, Data}.

%%------------------------------------------------------------------------------
%% Tests
%%------------------------------------------------------------------------------

formated_empty_test() ->
    Data = <<1,2,3,4>>,
    ?assert({error, Data} =:= correctly_formated(Data)).

formated_good_no_extra_test() ->
    Token = <<1,2,3,4>>,
    Payload = mm_encode:login(Token),
    ?assert({ok, ?AUTH, Token, <<>>} =:= correctly_formated(Payload)).

formated_good_with_extra_test() ->
    Token = <<1,2,3,4>>,
    Part = mm_encode:login(Token),
    Payload = <<Part/binary, Part/binary>>,
    ?assert({ok, ?AUTH, Token, Part} =:= correctly_formated(Payload)).
