-module(login).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(SIZE_RANDOM_GOD_TOKEN, 20).

-export([start_link/0,
	 attempt/1,
	 add_token/1]).
-export([may_token_publish_in_room/2,
	 may_token_subscribe_in_room/2]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

attempt(Token) ->
    gen_server:call(?MODULE, {attempt, Token}).

add_token(Token) ->
    gen_server:cast(?MODULE, {add, Token}).

may_token_publish_in_room(Token, Room) ->
    ok.

may_token_subscribe_in_room(Token, Room) ->
    ok.

init(_) ->
    Random = base64:encode(crypto:strong_rand_bytes(?SIZE_RANDOM_GOD_TOKEN)),
    Token = mm_support:binary_env_var("god_token", Random),
    ets:new(?MODULE, [set, named_table]),
    ets:insert(?MODULE, {Token, true}),
    {ok, #{god => Token}}.

handle_call({attempt, Token}, _From, State=#{god := Token}) ->
    {reply, ok, State};

handle_call({attempt, Token}, _From, State) ->
    case ets:lookup(?MODULE, Token) of
	[{Key, true}] ->
	    {reply, ok, State};
	_ ->
	    {reply, error, State}
    end;

handle_call(What, _From, State) ->
    {reply, {eror, What}, State}.

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
