%%%-------------------------------------------------------------------
%% @doc MiniMonkey's lowest level protocol
%%
%% The purpose is simply to follow the basic protocol that we have
%% one byte representing the code and two bytes representing the size
%% of the optional payload.
%% @end
%%%-------------------------------------------------------------------

-module(minimonkey_protocol).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/3]).

start_link(Ref, _Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
    {ok, Pid}.

init(Ref, Transport, _Opts = []) ->
    {ok, Socket} = ranch:handshake(Ref),
    outer(Socket, Transport).

%%------------------------------------------------------------------------------
%% Private
%%------------------------------------------------------------------------------

outer(Socket, Transport) ->
    case Transport:recv(Socket, 3, 5000) of
	{ok, <<Code:8, Size:16/little>>} ->
	    inner(Socket, Transport, Code, Size);
	_ ->
	    ok = Transport:close(Socket)
     end.

inner(Socket, Transport, Code, 0) ->
    minimonkey:process(Code, <<>>);
inner(Socket, Transport, Code, Size) ->
    case Transport:recv(Socket, Size, 5000) of
	{ok, Payload} ->
	    minimonkey:process(Code, Payload);
	_  ->
	    ok = Transport:close(Socket)
    end.
