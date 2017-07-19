%%%-------------------------------------------------------------------
%%% @author Rick  Payne <rickp@OtoloNetworks.com>
%%% @copyright (C) 2017, Rick Payne
%%% @doc
%%%
%%% A VRRP instance supervisor. Instantiate a VRRP FSM child to run
%%% the protocol FSM and an interface handler to send/receive VRRP
%%% packets and maniuplate the ARP tables etc.
%%%
%%% @end
%%% Created :  7 Aug 2017 by Rick  Payne <rickp@OtoloNetworks.com>
%%%-------------------------------------------------------------------
-module(vrrp_instance_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, stop/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
    supervisor:start_link(?MODULE, Args).

%%--------------------------------------------------------------------
%% @doc
%% Stops the supervisor and any child processes
%%
%% @spec stop(Pid) -> true.
%% @end
%%--------------------------------------------------------------------
stop(Pid) ->
    %% Normal termination, the supervisor logic handles informing the
    %% child processes.
    erlang:exit(Pid, normal).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init(Args) ->

    SArgs = Args ++ [{supervisor, self()}],

    SupFlags = #{strategy => one_for_all,
                 intensity => 1,
                 period => 5},

    FSMChild = #{id => fsm,
                 start => {vrrp_fsm, start_link, [SArgs]},
                 restart => permanent,
                 shutdown => 5000,
                 type => worker,
                 modules => [vrrp_fsm]},

    {ok, {SupFlags, [FSMChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
