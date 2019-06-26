-module(minimonkey).
-include("codes.hrl").

-export([process/2]).

process(?AUTH, Token) ->
    case login:attempt(Token) of
	ok ->
	    ok;
	_ ->
	    close
    end;

process(?ENTER, RoutingKey) ->
    %% rooms_sup:create_room(RoutingKey),
    ok.
