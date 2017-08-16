%%%-------------------------------------------------------------------
%%% @author Rick  Payne <rickp@OtoloNetworks.com>
%%% @copyright (C) 2017, Otolo Networks, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 18 Jul 2017 by Rick  Payne <rickp@OtoloNetworks.com>
%%%-------------------------------------------------------------------
-module(vrrp_fsm).

-behaviour(gen_fsm).

-include("vrrp_protocol.hrl").

%% API
-export([start_link/1]).

%% Public API
-export([stop/1, vrrp_msg/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).

%% States for our model
-export(['INITIALISE'/2, 'MASTER'/2, 'BACKUP'/2]).


-define(SERVER, ?MODULE).

-record(state, {
          our_ip,        %% Configured IP address, as an integer
          family :: ipv4 | ipv6,
          supervisor,    %% Supervisor reference
          interface,     %% Interface on which we are running
          interface_pid, %% Pid of interface handler
          id,            %% VRRP ID
          vip = [],      %% List of Virtual IPs (all same family)
          priority = ?VRRP_PRIORITY_DEFAULT, %% Virtual Router Priority
          interval = ?VRRP_ADVERT_INTERVAL_DEFAULT,
          preempt_mode = true, %% Do we take over?
          master_adver_interval,
          master_down_interval = ?VRRP_ADVERT_INTERVAL_DEFAULT,
          %% Timers
          master_down_timer = undef,
          adver_timer = undef
         }).

%%%===================================================================
%%% API
%%%===================================================================
vrrp_msg(Pid, #vrrp_packet{} = Msg) ->
    gen_fsm:send_event(Pid, {message, Msg}).

stop(VId) ->
    Pid = map_processid(VId),
    gen_fsm:stop(Pid).

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
    Id = proplists:get_value(id, Args),
    Name = erlang:list_to_atom("vrrp_" ++ erlang:integer_to_list(Id)),
    Keys = proplists:get_keys(Args),
    case [interface, id, ip, supervisor] -- Keys of
        [] ->
            gen_fsm:start_link({local, Name}, ?MODULE, Args, []);
        Missing ->
            {error, {missing, Missing}}
    end.

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init(Args) ->
    process_flag(trap_exit, true),

    Supervisor = proplists:get_value(supervisor, Args),
    VId = proplists:get_value(id, Args),
    IPs = proplists:get_value(ip, Args),
    Interface = proplists:get_value(interface, Args),
    Priority = proplists:get_value(priority, Args, ?VRRP_PRIORITY_DEFAULT),
    Interval = proplists:get_value(interval, Args, ?VRRP_ADVERT_INTERVAL_DEFAULT),
    Preempt = proplists:get_value(preempt, Args, true),
    case set_family(IPs) of
        {error, mixed} ->
            {stop, "Mix of protected addresses"};
        Family ->
            {ok, 'INITIALISE',
             #state{
                supervisor = Supervisor,
                interface = Interface,
                id = VId,
                family = Family,
                vip = IPs,
                priority = Priority,
                interval = Interval,
                preempt_mode = Preempt,
                master_down_interval = Interval
               },
             0}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
'INITIALISE'(timeout, #state{priority = 255} = State) ->
    {next_state, 'MASTER', become_master(register_interface(State))};
'INITIALISE'(timeout, State) ->
    {next_state, 'BACKUP', become_backup(register_interface(State))};
'INITIALISE'({message, _M}, State) ->
    %% Ignore messages before we've started
    {next_state, 'INITIALISE', State}.

%% 'MASTER'({message, #vrrp_packet{version = 3, type = announce, id = 
'MASTER'({message, M}, State) ->
    handle_message('MASTER', M, State);
'MASTER'({adver_timer}, State) ->
    {next_state, 'MASTER', send_advert(State)};
'MASTER'(_Event, State) ->
    {next_state, 'MASTER', State}.

'BACKUP'({message, M}, State) ->
    handle_message('BACKUP', M, State);
'BACKUP'({master_down_timer}, State) ->
    {next_state, 'MASTER', become_master(State)};
'BACKUP'(_Event, State) ->
    {next_state, 'BACKUP', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info({timeout, _, Timer}, StateName, State) ->
    gen_fsm:send_event(self(), Timer),
    {next_state, StateName, State};
handle_info(Info, StateName, State) ->
    io:format("Handling ~p~n", [Info]),
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, 'MASTER', State) ->
    %% Resign, then shutdown..
    send_advert(State#state{priority = 0}),
    vrrp_interface_manager:remove(State#state.interface, State#state.id),
    ok;
terminate(_Reason, _Statename, State) ->
    vrrp_interface_manager:remove(State#state.interface, State#state.id),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% Sanity check the incoming message...
%%% ===================================================================
handle_message(StateName, #vrrp_packet{from = AIP, id = I} = M, #state{our_ip = LIP, id = I} = State)
  when AIP =/= LIP ->
    process_message(StateName, M, State);
handle_message(StateName, _M, S) ->
    %% VRRP ID does not match or the packet was from us, so we ignore
    {next_state, StateName, S}.


%%%===================================================================
%%% The main logic section where we encode most of the work of the
%%% VRRP state machine.
%%% ===================================================================
%% For backup
   %% (425) + If the Priority in the ADVERTISEMENT is zero,
   %%     then:
   %%   (430) * Set the Master_Down_Timer to Skew_Time
   %% (440) + else // priority non-zero
   %%    (445) * If Preempt_Mode is False,
   %%           or if the Priority in the
   %%          ADVERTISEMENT is greater than or equal to the local
   %%          Priority,
   %%        then:
   %%      (450) @ Set Master_Adver_Interval to Adver Interval
   %%             contained in the ADVERTISEMENT
   %%      (455) @ Recompute the Master_Down_Interval
   %%      (460) @ Reset the Master_Down_Timer to
   %%             Master_Down_Interval
   %%    (465) * else // preempt was true or priority was less
   %%       (470) @ Discard the ADVERTISEMENT
   %%    (475) *endif // preempt test
   %% (480) +endif // was priority zero?
process_message('BACKUP', #vrrp_packet{priority = 0, interval = I},
                #state{priority = P} = State) ->
    %% 430 - 0 priority, so we set our MDT to Skew Time
    {next_state, 'BACKUP',
     start_timer(master_down_timer,
                 State#state{master_down_interval = skew_time(P, I)})};
process_message('BACKUP', #vrrp_packet{priority = AP, interval = AI},
                #state{preempt_mode = SPM, priority = LP} = State)
    when SPM =:= false; AP >= LP ->
    %% 445 Prempt is false, or Advert is greater then or equal local priority
    {next_state, 'BACKUP',
     start_timer(master_down_timer,
                 State#state{
                   master_adver_interval = AI,
                   master_down_interval = master_down_time(AI, LP)})};
process_message('BACKUP', #vrrp_packet{}, #state{} = State) ->
    %% 465 - discard...
    {next_state, 'BACKUP', State};

%% For the Master:
   %% (705) -+ If the Priority in the ADVERTISEMENT is zero, then:
   %%    (710) -* Send an ADVERTISEMENT
   %%    (715) -* Reset the Adver_Timer to Advertisement_Interval
   %% (720) -+ else // priority was non-zero
   %%    (725) -* If the Priority in the ADVERTISEMENT is greater
   %%          than the local Priority,
   %%    (730) -* or
   %%    (735) -* If the Priority in the ADVERTISEMENT is equal to
   %%       the local Priority and the primary IPvX Address of the
   %%       sender is greater than the local primary IPvX Address, then:
   %%       (740) -@ Cancel Adver_Timer
   %%       (745) -@ Set Master_Adver_Interval to Adver Interval
   %%          contained in the ADVERTISEMENT
   %%       (750) -@ Recompute the Skew_Time
   %%       (755) @ Recompute the Master_Down_Interval
   %%       (760) @ Set Master_Down_Timer to Master_Down_Interval
   %%       (765) @ Transition to the {Backup} state
   %%    (770) * else // new Master logic
   %%       (775) @ Discard ADVERTISEMENT
   %%    (780) *endif // new Master detected
   %% (785) +endif // was priority zero?
process_message('MASTER', #vrrp_packet{priority = 0}, #state{} = State) ->
    %% 710
    send_advert(State),
    {next_state, 'MASTER', start_timer(adver_timer, State)};
process_message('MASTER', #vrrp_packet{priority = AP, interval = AI, from = AIP},
                #state{priority = LP, our_ip = LIP} = State)
    when AP > LP orelse (AP == LP andalso AIP > LIP) ->
    stop_timer(State#state.adver_timer),
    io:format("Becoming BACKUP for ~p~n", [State#state.id]),
    {next_state, 'BACKUP',
     become_backup(State#state{
                     adver_timer = undef,
                     master_adver_interval = AI,
                     master_down_interval = master_down_time(AI, AP)})};
process_message('MASTER', #vrrp_packet{priority = AP, from = AIP}, #state{priority = LP, our_ip = LIP} = State) ->
    io:format("AIP: ~p LIP: ~p~nAP: ~p LP ~p~n",
              [AIP, LIP, AP, LP]),
    %% Nothing to do...
    {next_state, 'MASTER', State}.

register_interface(#state{interface = I, id = V} = State) ->
    {ok, InterfacePid, IP} = vrrp_interface_manager:set(I, V, self()),
    <<IPInt:32>> = list_to_binary(tuple_to_list(IP)),
    erlang:link(InterfacePid),
    io:format("Our IP: ~p~n", [IPInt]),
    State#state{interface_pid = InterfacePid, our_ip = IPInt}.

become_master(#state{family = ipv4} = State) ->
    io:format("Becoming Master for ID ~p~n", [State#state.id]),
    %% Send gratuitous arp
    %% send_advert
    send_advert(State);
become_master(#state{family = ipv6} = State) ->
    io:format("Becoming Master for ID ~p~n", [State#state.id]),
    %% Send unsolicited ND
    %% Start adver_timer
    start_timer(adver_timer, State).

become_backup(#state{family = ipv4} = State) ->
    %% Remove MAC from tables
    %% Start master_down_timer
    start_timer(master_down_timer, State).

send_advert(State) ->
    %% Send Advert
    vrrp_interface:send_message(State#state.interface_pid,
                                #vrrp_packet{
                                   id = State#state.id,
                                   priority = State#state.priority,
                                   interval = State#state.interval,
                                   ips = State#state.vip
                                  }),
    %% Start timer..
    start_timer(adver_timer, State).

start_timer(master_down_timer, State) ->
    stop_timer(State#state.master_down_timer),
    Timeout = erlang:trunc(State#state.master_down_interval * 10),
    Timer = erlang:start_timer(Timeout, self(), {master_down_timer}),
    State#state{master_down_timer = Timer};
start_timer(adver_timer, State) ->
    stop_timer(State#state.adver_timer),
    %% Interval is in centiseconds and we want it in miliseconds...
    Timeout = erlang:trunc(State#state.interval * 10),
    Timer = erlang:start_timer(Timeout, self(), {adver_timer}),
    State#state{adver_timer = Timer}.

stop_timer(undef) ->
    ok;
stop_timer(Ref) when is_reference(Ref) ->
    erlang:cancel_timer(Ref).

map_processid(VId) ->
    erlang:whereis(erlang:list_to_atom("vrrp_" ++ erlang:integer_to_list(VId))).

master_down_time(I, P) ->
    erlang:trunc(((3 * I) + skew_time(P, I))).

skew_time(P, I) ->
    (((256 - P) * I) / 256).

set_family(IPs) ->
    Fs =
        lists:usort(
          lists:map(
            fun({_, _, _, _}) ->
                    ipv4;
               ({_, _, _, _, _, _, _, _}) ->
                    ipv6
            end, IPs)),
    case length(Fs) of
        1 ->
            hd(Fs);
        _ ->
            {error, mixed}
    end.
