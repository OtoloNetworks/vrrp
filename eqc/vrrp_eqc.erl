%%%-------------------------------------------------------------------
%%% @author Rick  Payne <rickp@OtoloNetworks.com>
%%% @copyright (C) 2017, Otolo Networks, Inc
%%% @doc
%%%
%%% An EQC model for VRRP to test the FSM etc.
%%%
%%% @end
%%% Created : 18 Jul 2017 by Rick  Payne <rickp@OtoloNetworks.com>
%%%-------------------------------------------------------------------
-module(vrrp_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").

-include("packets.hrl").

-compile([export_all, nowarn_export_all]).

-compile({pulse_skip, [timer_sleep/1]}).

-define(VRRP_MULTICAST, {224, 0, 0, 18}).

%% -- Notes ------------------------------------------------------------------

%% [Note:ModelCoverage]
%%
%%  Modelled complete VRRP state machine, but no IPv6 or routing.
%%
%%  Not covered
%%    - IPv6
%%    - ARP/ND (115 - 135, 305 - 330, 375 - 400, 605 - 640)
%%    - Routing (335, 340, 645, 650)
%%    - Preempt_Mode = false (modelled but not tested)

%% [Note:ShutDown]
%%
%%  Shutting down a router with vrrp_fsm:shutdown/1 doesn't work since the
%%  supervisor restarts the fsm. In general, it would be nice to have a clear
%%  API for starting and stopping a router.

%% [Note:PriorityZeroAdvertInterval]
%%
%%  The spec (430) doesn't say to use the Adver Interval from the ADVERTISEMENT
%%  message with priority 0. It does make sense to do this though. The only way
%%  to have Master_Adver_Interval different from the Adver Interval in the message
%%  (assuming other routers are following the spec) is if we missed an
%%  ADVERTISEMENT from the master, for instance, if we are started just before
%%  the priority-zero message. In this case the routers who didn't miss the
%%  ADVERTISEMENT will use the Master_Adver_Interval of the master, but unless
%%  we look at the interval in the priority-zero advertisement, we are using
%%  our own Advertisement_Interval.
%%
%%  Both the model and the implementation are violating the spec in this way.

%% -- State ------------------------------------------------------------------

-record(state,
  { time    = 0
  , routers = []
  }).

-record(router_data,
  { state                   = initialize
  , timer                   = infinity
  , 'Master_Adver_Interval' = 100
  }).

-record(router,
  { id
  , ip_version = ipv4
  , ips = []
  , priority = 100
  , interface
  , read_socket
  , write_socket
  , local_ip
  , pid
  , router_pid
  , 'Preempt_Mode' = true
  , 'Accept_Mode' = false
  , 'Advertisment_Interval' = 100
  , modified_time
  , data = #router_data{}
  }).

initial_state() -> #state{}.

-define(SET(Id, Key, Val), ?APPLY(set_data, [Id, #router_data.Key, Val])).

get_router(S, Id) ->
  lists:keyfind(Id, #router.id, S#state.routers).

update_router(S, R) ->
  S#state{ routers = lists:keyreplace(R#router.id, #router.id, S#state.routers, R) }.

get_initial_router(S, Id) ->
  lists:keyfind(Id, #router.id, initial_routers(S)).

get_active_router(S, Id) ->
  lists:keyfind(Id, #router.id, active_routers(S)).

initial_routers(S) ->
  [ R || R = #router{ data = #router_data{ state = initialize } } <- S#state.routers ].

active_routers(S) ->
  [ R || R = #router{ data = #router_data{ state = RS } } <- S#state.routers, RS /= initialize ].

find_interface(S, Iface) ->
  case [ R || R = #router{ interface = Iface1, pid = Pid } <- S#state.routers,
              Iface == Iface1, Pid /= undefined ] of
    [R | _] -> R;
    _       -> false
  end.

'Skew_Time'(S, Id) ->
  'Skew_Time'(get_router(S, Id)).

'Skew_Time'(#router{ priority = Prio, data = Data }) ->
  Master_Adver_Interval = Data#router_data.'Master_Adver_Interval',
  (256 - Prio) * Master_Adver_Interval div 256.

'Master_Down_Interval'(S, Id) ->
  'Master_Down_Interval'(get_router(S, Id)).

'Master_Down_Interval'(R = #router{ data = Data }) ->
  Master_Adver_Interval = Data#router_data.'Master_Adver_Interval',
  3 * Master_Adver_Interval + 'Skew_Time'(R).

%% -- Operations -------------------------------------------------------------

%% --- init_router ---

init_router_args(_S) -> [gen_init_options()].

init_router_pre(S, [#{ id := Id, ips := Ips }]) ->
  not lists:keymember(Id, #router.id, S#state.routers) andalso
    lists:all(fun(R) -> disjoint(Ips, R#router.ips) end, S#state.routers).

init_router(_) -> ok. %% This is purely a model operation

init_router_callouts(_S, [#{ id := Id, priority := Prio, ips := Ips, interface := Iface }]) ->
  Router = #router{ id = Id
                  , priority = Prio
                  , ips = Ips
                  , interface = Iface },
  ?APPLY(add_router, [Router]).

%% --- startup ---

startup_pre(S) -> [] /= initial_routers(S).

startup_args(S) ->
  ?LET(R, elements(initial_routers(S)),
       [#{ id => R#router.id
         , priority => R#router.priority
         , ips => R#router.ips
         , interface => R#router.interface
         , mod_time => R#router.modified_time }]).

startup_pre(S, [#{ id := Id, priority := Prio, ips := Ips,
                          interface := Iface, mod_time := T }]) ->
  case get_initial_router(S, Id) of
    #router{ priority = Prio, ips = Ips, interface = Iface } ->
      T == undefined orelse T < S#state.time; %% Avoid race between stop and start
    _ -> false
  end.

startup_adapt(S, [Args = #{ id := Id }]) ->
  case get_initial_router(S, Id) of
    #router{ priority = Prio, ips = Ips, interface = Iface } ->
      [Args#{ priority := Prio, ips := Ips, interface := Iface }];
    _ -> false
  end.

startup(#{ id := Id, priority := Prio, ips := Ips, interface := Iface }) ->
  {ok, Pid} = vrrp_instance_sup:start_link([{id, Id}, {priority, Prio},
                                            {ip, Ips}, {interface, Iface}]),
  Pid.

startup_callouts(_S, [#{ id := Id }]) ->
  ?APPLY(start_interface, [Id]),
  ?APPLY(do_startup, [Id]).

startup_next(S, RouterPid, [#{ id := Id }]) ->
  R = get_router(S, Id),
  S#state{ routers = lists:keyreplace(Id, #router.id, S#state.routers,
                                      R#router{ router_pid = RouterPid }) }.

%% --- sleep ---

sleep_args(_S) -> [gen_sleep()].

sleep(N) ->
  timer:sleep(N * 10).

sleep_callouts(_, [N]) ->
  ?APPLY(advance_time, [N]).

%% --- recv_advert ---

recv_advert_pre(S) -> [] /= active_routers(S).

recv_advert_args(S) ->
  ?LET(R, elements(active_routers(S)),
  [ #{ id       => R#router.id
     , prio     => ?SUCHTHAT(P, gen_priority_z(), P < 255 orelse R#router.priority < 255)
     , interval => gen_adver_interval()
     , src_ip   => gen_ip()
     , ips      => return(R#router.ips)
     , pid      => R#router.pid
     , socket   => R#router.read_socket } ]).

recv_advert_pre(S, [#{id := Id, prio := Prio, ips := Ips, pid := Pid, socket := Socket}]) ->
  case get_active_router(S, Id) of
    #router{ ips = Ips, pid = Pid, read_socket = Socket, priority = RPrio, modified_time = T } ->
      T < S#state.time andalso    %% avoid race between init and send_adver
      (Prio < 255 orelse RPrio < 255);
    _ -> false
  end.

recv_advert_adapt(S, [Args = #{id := Id}]) ->
  case get_active_router(S, Id) of
    #router{ ips = Ips, pid = Pid, read_socket = Socket } ->
      [Args#{ ips := Ips, pid := Pid, socket := Socket }];
    _ -> false
  end.

recv_advert_callouts(S, [#{ id := Id, prio := Prio, interval := Int, src_ip := SrcIp }]) ->
  R = get_router(S, Id),
  case R#router.data#router_data.state of
    backup ->                                       %% (420)
      case Prio of
        0 ->                                        %% (425)
          ?SET(Id, 'Master_Adver_Interval', Int),   %% !! NOTE:PriorityZeroAdvertInterval
          ?APPLY(set_skew_timer, [Id]);             %% (430)
        _ when R#router.'Preempt_Mode' == false;
               Prio >= R#router.priority ->         %% (445)
          ?SET(Id, 'Master_Adver_Interval', Int),   %% (450)
          ?APPLY(set_master_down_timer, [Id]);      %% (455) (460)
        _ ->
          ?EMPTY                                    %% (470)
      end;
    master ->                             %% (700)
      case Prio of
        0 ->                              %% (705)
          ?APPLY(send_advert, [R]),       %% (710)
          ?APPLY(set_adver_timer, [Id]);  %% (715)
        _ when {Prio, SrcIp} > {R#router.priority, R#router.local_ip} -> %% (725) (730) (735)
          ?SET(Id, 'Master_Adver_Interval', Int),                        %% (745)
          ?APPLY(set_master_down_timer, [Id]),                           %% (740) (750) (755) (760)
          ?SET(Id, state, backup);                                       %% (765)
        _ -> ?EMPTY                                                      %% (775)
      end
  end.

recv_advert(#{id := Id, prio := Prio, interval := Int, src_ip := SrcIp, ips := Ips, pid := Pid, socket := Socket}) ->
  Pkt = #pkt{ version = 4
            , src = SrcIp
            , dst = ?VRRP_MULTICAST
            , packet = #vrrp_packet
              { id = Id
              , priority = Prio
              , max_adver_int = Int
              , ips = Ips } },
  gen_udp_mock:recv(Pid, Socket, SrcIp, 0, Pkt).

%% --- shutdown ---

shutdown_pre(S) -> [] /= active_routers(S).

shutdown_args(S) ->
  ?LET(R, elements(active_routers(S)), [#{ id => R#router.id, pid => R#router.router_pid }]).

shutdown_pre(S, [#{ id := Id, pid := Pid }]) ->
  case get_active_router(S, Id) of
    #router{ router_pid = Pid } -> true;
    _                           -> false
  end.

shutdown_adapt(S, [Args = #{ id := Id }]) ->
  case get_active_router(S, Id) of
    #router{ router_pid = Pid } -> [Args#{ pid := Pid }];
    _                           -> false
  end.

shutdown(#{ pid := Pid }) ->
  vrrp_instance_sup:stop(Pid).

%% NOTE: This doesn't work. See [Note:ShutDown].
%% shutdown(#{ id := Id }) ->
%%   vrrp_fsm:stop(Id).

shutdown_callouts(S, [#{ id := Id }]) ->
  R = get_router(S, Id),
  ?APPLY(set_modified_time, [Id]),
  case R#router.data#router_data.state of
    backup ->                       %% (345)
      ?SET(Id, timer, infinity),    %% (350)
      ?SET(Id, state, initialize);  %% (355)
    master ->                                           %% (655)
      ?SET(Id, timer, infinity),                        %% (660)
      ?APPLY(send_advert, [R#router{ priority = 0 }]),  %% (665)
      ?SET(Id, state, initialize)                       %% (670)
  end.

%% -- Local operations -------------------------------------------------------

add_router_next(S, _, [R]) ->
  S#state{ routers = [R | S#state.routers] }.

add_interface_next(S, _, [Id, RSock, WSock, Addr, Pid]) ->
  R  = get_router(S, Id),
  R1 = R#router{ read_socket = RSock
               , write_socket = WSock
               , local_ip = Addr
               , pid = Pid },
  update_router(S, R1).

set_timer_next(S, _, [Id, Time]) ->
  set_data_next(S, unused, [Id, #router_data.timer, Time + S#state.time]).

set_skew_timer_callouts(S, [Id]) ->
  R = get_router(S, Id),
  ?APPLY(set_timer, [Id, 'Skew_Time'(R)]).

set_master_down_timer_callouts(S, [Id]) ->
  R = get_router(S, Id),
  ?APPLY(set_timer, [Id, 'Master_Down_Interval'(R)]).

set_adver_timer_callouts(S, [Id]) ->
  R = get_router(S, Id),
  ?APPLY(set_timer, [Id, R#router.'Advertisment_Interval']).

set_data_next(S, _, [Id, Field, Val]) ->
  Old = lists:keyfind(Id, #router.id, S#state.routers),
  New = Old#router{ data = setelement(Field, Old#router.data, Val) },
  S#state{ routers = lists:keystore(Id, #router.id, S#state.routers, New) }.

start_interface_callouts(S, [Id]) ->
  #router{ interface = Iface } = get_router(S, Id),
  case find_interface(S, Iface) of
    #router{ read_socket = RSock, write_socket = WSock, local_ip = Addr, pid = Pid } ->
      ?APPLY(add_interface, [Id, RSock, WSock, Addr, Pid]); %% Interface already started
    _ ->
      ?MATCH({Addr, R, W}, ?CALLOUT(vrrp_socket, create_socket, [ipv4, Iface], {gen_ip(), gen_fd(), gen_fd()})),
      ?MATCH({ok, RSock},  ?CALLOUT(gen_udp, open, [0, [{fd, R}, binary]], {ok, gen_socket()})),
      ?MATCH({Pid, ok},    ?CALLOUT(gen_udp, controlling_process, [RSock, ?VAR], ok)),
      ?MATCH({ok, WSock},  ?CALLOUT(gen_udp, open, [0, [{fd, W}, binary]], {ok, gen_socket()})),
      ?CALLOUT(gen_udp, controlling_process, [WSock, Pid], ok),
      ?APPLY(add_interface, [Id, RSock, WSock, Addr, Pid])
  end.

do_startup_callouts(S, [Id]) ->                                            %% (100)
  R = get_router(S, Id),
  ?APPLY(set_modified_time, [Id]),
  case R#router.priority of
    255 ->                                                                 %% (105)
      ?APPLY(send_advert, [R]),                                            %% (110)
      case R#router.ip_version of
        ipv4 ->                                                            %% (115)
          ?EMPTY;                                                          %% (120) TODO
        ipv6 ->                                                            %% (125)
          ?FAIL(todo)                                                      %% (130) TODO
      end,
      ?APPLY(set_adver_timer, [Id]),                                       %% (140)
      ?SET(Id, state, master);                                             %% (145)
    _   ->                                                                 %% (150)
      ?SET(Id, 'Master_Adver_Interval', R#router.'Advertisment_Interval'), %% (155)
      ?APPLY(set_master_down_timer, [Id]),                                 %% (160)
      ?SET(Id, state, backup)                                              %% (165)
  end.

send_advert_callouts(_S, [R]) ->
  ?CALLOUT(gen_udp, send, [R#router.write_socket, ?VRRP_MULTICAST,
                           #pkt{ version = R#router.ip_version
                               , src = R#router.local_ip
                               , dst = ?VRRP_MULTICAST
                               , packet = #vrrp_packet
                                 { id = R#router.id
                                 , priority = R#router.priority
                                 , max_adver_int = R#router.'Advertisment_Interval'
                                 , ips = R#router.ips } }], ok).

next_timer(S) ->
  lists:min([ T || #router{ data = #router_data{ timer = T } } <- S#state.routers ] ++ [infinity]).

advance_time_callouts(_, [0]) -> ?EMPTY;
advance_time_callouts(S, [N]) ->
  T0 = S#state.time,
  case next_timer(S) of
    T when T > T0 + N -> ?APPLY(set_time, [T0 + N]);
    T ->
      ?APPLY(set_time, [T]),
      ?APPLY(check_timers, []),
      ?APPLY(advance_time, [N - (T - T0)])
  end.

set_time_next(S, _, [T]) ->
  S#state{ time = T }.

set_modified_time_next(S, _, [Id]) ->
  R = get_router(S, Id),
  S#state{ routers = lists:keyreplace(Id, #router.id, S#state.routers,
                                      R#router{ modified_time = S#state.time }) }.

check_timers_callouts(S, []) ->
  ?PAR([ ?APPLY(check_timer, [R]) || R <- S#state.routers ]).

check_timer_callouts(S, [R]) ->
  Id = R#router.id,
  #router_data{ state = State, timer = Timer } = R#router.data,
  case State of
    _ when S#state.time < Timer -> ?EMPTY;
    initialize -> ?FAIL(error);
    master ->                                 %% (680)
      ?APPLY(send_advert, [R]),               %% (685)
      ?APPLY(set_adver_timer, [Id]);          %% (690)
    backup ->                                 %% (365)
      ?APPLY(send_advert, [R]),               %% (370)
      case R#router.ip_version of
        ipv4 ->                               %% (375)
          ?EMPTY;                             %% (380) TODO
        ipv6 ->                               %% (385)
          ?FAIL(todo)                         %% (390, 395) TODO
      end,
      ?APPLY(set_adver_timer, [Id]),          %% (405)
      ?SET(Id, state, master)                 %% (410)
  end.

%% -- Generators -------------------------------------------------------------

gen_init_options() ->
  #{ id =>        gen_vrrp_id()
   , priority =>  gen_priority()
   , ips =>       gen_ips()
   , interface => gen_interface() }.

gen_vrrp_id()    -> choose(1, 5).
gen_priority()   -> weighted_default({1, 255}, {3, choose(1, 254)}).
gen_priority_z() -> weighted_default({5, gen_priority()}, {1, 0}).
gen_ips()        -> non_empty(list(gen_ip())).
gen_ip()         -> noshrink({10, 0, gen_byte(), gen_byte()}).
gen_interface()  -> elements(["br0", "en0", "en1"]).
gen_byte()       -> choose(0, 255).
gen_fd()         -> noshrink(choose(3, 100)).
gen_socket()     -> noshrink(choose(0, 10000)).

gen_adver_interval() -> choose(10, 200).

gen_sleep() ->
  ?LET(T, noshrink(frequency(
          [ {1, choose(  1,   10)}
          , {1, choose( 10,  100)}
          , {1, choose(100, 1000)} ])),
    eqc_gen:shrink_int(1, 1000, T)).


%% -- Helper functions -------------------------------------------------------

disjoint(Xs, Ys) ->
  (Xs -- Ys) == Xs.

%% -- Common -----------------------------------------------------------------

%% postcondition_common(S, Call, V) ->
%%   eq(V, return_value(S, Call)).

%% -- Custom shrinking -------------------------------------------------------
shrink_commands(CmdGen) ->
  ?LET(Cmds, shrink_commands1(CmdGen),
    pulse_component:adapt_commands(?MODULE, Cmds)).

-define(SHRINK_LAZY(G, Gs), eqc_gen:shrink_lazy(G, ?DELAY(Gs))).

shrink_commands1(CmdGen) ->
  ?LET(Cmds, CmdGen, shrink_commands2(Cmds)).

shrink_commands2(Cmds) ->
  ?SHRINK_LAZY(return(Cmds),
    eqc_lazy_lists:lazy_map(fun shrink_commands1/1,
                            eqc_lazy_lists:lazy_list(custom_shrink(Cmds)))).

%% Cmds -> [Cmds]
custom_shrink(Cmds) ->
  eqc_shrinking:sublist(2, Cmds,
    fun
      ([{set, V, {call, M, sleep, [T1], Meta1}},
         {set, _V, {call, M, sleep, [T2], _Meta2}}]) ->
         [[{set, V, {call, M, sleep, [T1 + T2], Meta1}}]];
       ([Cmd1 = {set, _, {call, _, sleep, _, _}},
         Cmd2 = {set, _, {call, _, NotSleep, _, _}}]) when NotSleep /= sleep ->
         [[Cmd2, Cmd1]];
       (_) ->
         []
    end) ++
  eqc_shrinking:sublist(1, Cmds,
    fun([{set, V, {call, M, sleep, [T], Meta}}]) when T > 100 ->
        [[{set, V, {call, M, sleep, [T - 100], Meta}}]];
      (_) ->
        []
    end).

%% -- Property ---------------------------------------------------------------
weight(_S, recv_advert) -> 5;
weight(_S, startup)     -> 3;
weight(_S, sleep)       -> 3;
weight(_, _)            -> 1.

prop_ok() -> prop_ok(?MODULE).
prop_ok(Mod) ->
  with_parameter(color, true,
  with_parameter(print_counterexample, false,
  with_parameter(default_process, worker,
  ?FORALL(Cmds, shrink_commands(pulse_component:commands(Mod)),
  ?LET(Shrinking, parameter(shrinking, false),
  ?ALWAYS(if Shrinking -> 5; true -> 1 end,
  ?FORALL(Seed, pulse:seed(),
  begin
    error_logger:tty(false),
    HSR={_, _, Res} = pulse:run(fun() ->
        process_flag(trap_exit, true),
        pulse_mocking:start_mocking(api_spec()),
        catch pulse_application:stop(vrrp),
        pulse_application:ensure_started(vrrp),
        HSR = pulse_component:run_commands(Mod, Cmds, [{command_timeout, 300000}]),
        catch pulse_application:stop(vrrp),
        timer:sleep(24 * 3600 * 1000),
        pulse_mocking:stop_mocking(),
        HSR
      end, [{seed, Seed}, single_mailbox, {strategy, unfair}, {run_timeout, 2000}]),
    ?WHENFAIL(timer_sleep(100), %% let pulse:verbose() output print before pretty_commands
    pulse_component:pretty_commands(Mod, Cmds, HSR,
    check_command_names(Mod, Cmds,
      Res == ok)))
  end))))))).

setup()   -> pulse_mocking:start_mocking(api_spec()).
cleanup() -> ok.

timer_sleep(N) ->
  timer:sleep(N).

%% -- API spec ---------------------------------------------------------------

api_spec() ->
  #api_spec
  { mocking  = pulse_mocking
  , language = erlang
  , modules  =
    [ #api_module
      { name      = gen_udp
      , fallback  = gen_udp_mock
      , functions =
        [ #api_fun{ name = open, arity = 2 }
        , #api_fun{ name = controlling_process, arity = 2}
        , #api_fun{ name = send, arity = 3}
        ]
      }
    , #api_module
      { name      = vrrp_socket
      , functions = [ #api_fun{ name = create_socket, arity = 2 } ] }
    ] }.

