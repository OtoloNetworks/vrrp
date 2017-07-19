%%%-------------------------------------------------------------------
%% @doc vrrp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(vrrp_sup).

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

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->

    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    IMChild = #{id => interface_manager,
                start => {vrrp_interface_manager, start_link, []},
                restart => permanent,
                shutdown => 5000,
                type => worker,
                modules => [vrrp_interface_manager]},

    {ok, { SupFlags, [IMChild]}}.

%%====================================================================
%% Internal functions
%%====================================================================
