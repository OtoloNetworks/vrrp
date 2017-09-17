%%%-------------------------------------------------------------------
%% @doc vrrp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(vrrp_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_vrrp/1, stop_vrrp/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_vrrp(Args) ->
    Id = proplists:get_value(id, Args),
    Name = erlang:list_to_atom("vrrp_" ++ erlang:integer_to_list(Id)),
    {ok, Child} =
        supervisor:start_child(?SERVER,
                               #{id => Name,
                                 start => {vrrp_instance_sup, start_link, [Args]},
                                 restart => permanent,
                                 shutdown => 5000,
                                 type => supervisor
                                }),
    {ok, Child, Name}.

stop_vrrp(Id) ->
    ok = supervisor:terminate_child(?SERVER, Id),
    ok = supervisor:delete_child(?SERVER, Id),
    ok.

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
