-module(mock_user).
-behaviour(gen_server).

-export([start_link/0,
	 messages/1]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

messages(User) ->
    gen_server:call(User, messages, 200).

init(_) ->
    {ok, []}.

handle_call(messages, _From, Messages) ->
    {reply, lists:reverse(Messages), Messages};

handle_call(What, _From, State) ->
    {reply, {error, What}, State}.

handle_cast(_What, State) ->
    {noreply, State}.

handle_info(Message, Messages) ->
    {noreply, [Message|Messages]}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
