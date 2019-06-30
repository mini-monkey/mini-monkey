%%%-------------------------------------------------------------------
%% @doc minimonkey top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(minimonkey_sup).
-behaviour(supervisor).
-define(SIZE_RANDOM_GOD_TOKEN, 20).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Random = base64:encode(crypto:strong_rand_bytes(?SIZE_RANDOM_GOD_TOKEN)),
    Token = mm_support:binary_env_var("god_token", Random),

    warn_if_random_god_token(Random, Token),

    ListenerSpec = ranch:child_spec(minimonkey, 100,
				    ranch_tcp, [{port, 1773}],
				    mm_user, []),
    Login = #{id => login,
	      start => {login, start_link, [Token]}},

    RoomSup = #{id => mm_room_sup,
		start => {mm_room_sup, start_link, [Token]}},

    Children = [ListenerSpec,
		RoomSup,
		Login],

    {ok, {{one_for_one, 10, 10}, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================

warn_if_random_god_token(Token, Token) ->
    lager:warning("god token is randomized");
warn_if_random_god_token(_, _) ->
    ok.
