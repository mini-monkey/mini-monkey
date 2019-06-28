%%%-------------------------------------------------------------------
%% @doc room top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(mm_room_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
	 create_room/1,
	 unname_room/1,
	 name_to_room/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

create_room(Name) ->
    case name_to_room(Name) of
	missing ->
	    {ok, Room} = supervisor:start_child(?MODULE, [Name]),
	    ets:insert(?MODULE, {Name, Room}),
	    {ok, Room};

	{ok, Room} ->
	    {ok, Room}
    end.

unname_room(Name) ->
    ets:delete(?MODULE, Name).

name_to_room(Name) ->
    case ets:lookup(?MODULE, Name) of
	[{Name, Room}] ->
	    {ok, Room};
	_ ->
	    missing
    end.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    ets:new(?MODULE, [set,
		      public,
		      named_table,
		      {read_concurrency, true},
		      {write_concurrency, true}]),

    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 1,
                 period => 1},

    Room = #{id => mm_room,
	     restart => transient,
	     start => {mm_room, start_link, []}},

    Children = [Room],

    {ok, {SupFlags, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
