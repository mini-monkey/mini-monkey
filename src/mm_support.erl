-module(mm_support).

-export([binary_env_var/2]).

binary_env_var(Name, Default) when is_list(Default) ->
    binary_env_var(Name, list_to_binary(Default));

binary_env_var(Name, Default) ->
    case os:getenv(Name) of
	false ->
	    Default;
	Var ->
	    list_to_binary(Var)
    end.
