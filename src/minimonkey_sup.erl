%%%-------------------------------------------------------------------
%% @doc minimonkey top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(minimonkey_sup).

-behaviour(supervisor).

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
    RanchSupSpec = {ranch_sup, {ranch_sup, start_link, []},
		    permanent, 5000, supervisor, [ranch_sup]},
    ListenerSpec = ranch:child_spec(minimonkey, 100,
				    ranch_tcp, [{port, 1773}],
				    minimonkey_protocol, []),
    Login = #{id => login,
	      start => {login, start_link, []}},

    {ok, {{one_for_one, 10, 10}, [RanchSupSpec, ListenerSpec, Login]}}.

%%====================================================================
%% Internal functions
%%====================================================================
