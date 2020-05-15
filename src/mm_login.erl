%%%-------------------------------------------------------------------
%% @doc MiniMonkey Login Module.
%%
%% Only handle when users connect - not room permissions
%%
%% @end
%%%-------------------------------------------------------------------

-module(mm_login).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([start_link/1,
	 attempt/1,
	 add_token/1,
	 is_god_token/1]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

start_link(Token) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Token, []).

attempt(Token) ->
    gen_server:call(?MODULE, {attempt, Token}).

add_token(Token) ->
    gen_server:cast(?MODULE, {add, Token}).

is_god_token(Token) ->
    gen_server:call(?MODULE, {is_god_token, Token}).

init(Token) ->
    ets:new(?MODULE, [set, named_table]),
    ets:insert(?MODULE, {Token, true}),
    {ok, #{god => Token}}.

handle_call({attempt, Token}, _From, State=#{god := Token}) ->
    {reply, ok, State};

handle_call({attempt, Token}, _From, State) ->
    case ets:lookup(?MODULE, Token) of
	[{Token, true}] ->
	    {reply, ok, State};
	_ ->
	    {reply, error, State}
    end;

handle_call({is_god_token, Token}, _From, State=#{god := GodToken}) ->
    {reply, Token =:= GodToken, State};

handle_call(What, _From, State) ->
    {reply, {error, What}, State}.

handle_cast({add, Token}, State) ->
    ets:insert(?MODULE, {Token, true}),
    {noreply, State};

handle_cast(_What, State) ->
    {noreply, State}.

handle_info(_What, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
