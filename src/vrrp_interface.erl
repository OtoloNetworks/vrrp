%%%-------------------------------------------------------------------
%%% @author Rick  Payne <rickp@OtoloNetworks.com>
%%% @copyright (C) 2017, Rick Payne
%%% @doc
%%%
%%% @end
%%% Created :  7 Aug 2017 by Rick Payne <rickp@OtoloNetworks.com>
%%%-------------------------------------------------------------------
-module(vrrp_interface).

-behaviour(gen_server).

-include("vrrp_protocol.hrl").

-define(V4_HEADER, 20).
-define(INT16MAX, 65535).

%% API
-export([start_link/1]).

-export([add_mapping/3, remove_mapping/2,
         send_message/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          interface,   %% Physical interface we are running on
          v4_addr,
          v4_rsocket,
          v4_wsocket,
          mapping      %% Dict of Id -> FSM Pid
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
add_mapping(Interface, Id, FSMPid) ->
    gen_server:call(Interface, {add_mapping, Id, FSMPid}).

remove_mapping(Interface, Id) ->
    gen_server:call(Interface, {remove_mapping, Id}).

send_message(Interface, Message) ->
    gen_server:call(Interface, {send, Message}).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Args) ->
    process_flag(trap_exit, true),
    Interface = proplists:get_value(interface, Args),

    %% This is a bit ugly! We use a NIF to create the sockets and then
    %% we trick gen_udp into giving us the data, including the source
    %% address (i.e. recvfrom()).
    {Addr, R, W} = vrrp_socket:create_socket(ipv4, Interface),
    {ok, RSocket} = gen_udp:open(0, [{fd, R}, binary]),
    ok = gen_udp:controlling_process(RSocket, self()),

    %% Not sure we need to do this bit? We need the fd, but do we need the gen_udp palava?
    {ok, WSocket} = gen_udp:open(0, [{fd, W}, binary]),
    ok = gen_udp:controlling_process(WSocket, self()),

    %%%
    io:format("Local interface address: ~p~n", [Addr]),

    %%% Rinse and repeat for the v6 and writer sockets when supported...

    %% Done
    {ok, #state{
            interface = Interface,
            v4_addr = Addr,
            v4_rsocket = RSocket,
            v4_wsocket = WSocket,
            mapping = dict:new()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({add_mapping, Id, FSMPid}, _From,
           #state{mapping = D, v4_addr = IP} = State) ->
    case dict:find(Id, D) of
        {ok, {_P, MR}} ->
            erlang:demonitor(MR);
        _ ->
            ok
    end,
    Ref = erlang:monitor(process, FSMPid),
    {reply, IP, State#state{mapping = dict:store(Id, {FSMPid, Ref}, D)}};
handle_call({remove_mapping, Id}, _From,
           #state{mapping = D} = State) ->
    ND =
        case dict:find(Id, D) of
            error ->
                D;
            {ok, {_P, MR}} ->
                erlang:demonitor(MR),
                dict:erase(Id, D)
        end,
    %% If the dict is empty, then exit...
    %% case dict:is_empty(ND) of
    %%     true ->
    %%         {stop, normal, State};
    %%     _ ->
    %%         {reply, ok, State#state{mapping = ND}}
    %% end.
    {reply, ok, State#state{mapping = ND}};
handle_call({send, V}, _From, State) ->
    {Reply, NewState} = send(V, State),
    {reply, Reply, NewState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, shutdown}, #state{mapping = D} = State) ->
    %% One of our FSM's has shutdown normally...
    ND = dict:filter(fun(_K, {VP, _VR}) -> VP =/= Pid end, D),
    {noreply, State#state{mapping = ND}};
handle_info({udp, _Socket, Source, _Zero, Pkt}, State) ->
    {noreply, handle_packet(verify_packet(Source, Pkt, State))};
handle_info(Info, State) ->
    io:format("vrrp_interface received: ~p~n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% Verify Packet
%%% Pattern match to parse the IP part of the packet and if thats ok,
%%% then go onto verify the VRRP part...
%%% TTL must be 255, Protocol must be 112.
%%%===================================================================
verify_packet(Source,
              <<4:4/integer, 5:4/integer, _:8, Len:16/big-integer,
                _:16, _:2, _:1, 0:13, 255:8, ?VRRP_PROTOCOL:8/integer,
                _:16, Src_Ip:32/big-integer, Dst_Ip:32/big-integer,
                Remainder/binary>> = Pkt,
              State)
  when byte_size(Pkt) =:= Len ->
    %% Initial parse...
    
    %% <<4:4/integer,          % Ip Version
    %%  5:4/integer,      % Header Length
    %%  _:8/integer,           % Tos -- Ignored
    %%  Len:16/big-integer,    % Packet Length
    %%  _:16/big-integer, % Fragment Id
    %%  _:2/integer,           % Two first flags... ignored
    %%  Mf:1/integer,          % More Fragments flag
    %%  Offset:13/big-integer, % Fragment Offset
    %%  255:8/integer,           % TTL -- We are not routing(yet) so we don't care
    %%  112:8/integer,    % Upper layer protocol
    %%  _:16/big-integer,      % Checksum... Alredy checked
    %%  Src_Ip:32/big-integer, % Source Ip
    %%  Dst_Ip:32/big-integer, % Should check this against our Ip
    %%  Remainder/binary>> = Packet, % We don't do options yet 

    PseudoHeader =
        <<Src_Ip:32/big-integer,
          Dst_Ip:32/big-integer,
          0:8,
          ?VRRP_PROTOCOL:8,
          (Len - 20):16/big-integer>>,

    case 0 == checksum_1([PseudoHeader, Remainder]) of
        true ->
            verify_vrrp(Source, Remainder, State);
        _ ->
            {bad_checksum, State}
    end;
verify_packet(_Source, _Packet, State) ->
    io:format("Z~n", []),
    {bad_packet, State}.

verify_vrrp(Source,
            <<3:4, 1:4, ID:8/integer, Priority:8/integer, Count:8/integer, 
              0:4, Interval:12/big-integer, _CSum:16/big-integer,
              BinIPs/binary>>,
            State)
 when Count == (byte_size(BinIPs)/4)->
    %% Map IPs into something useful...
    IPs = [ {A, B, C, D} || <<A:8, B:8, C:8, D:8>> <= BinIPs ],

    %% Map Source into an integer (used for comparism)
    SourceInt = vrrp_fsm:ip_to_int(Source),

    {#vrrp_packet{
        from = SourceInt,
        version = 3,
        type = announce,
        id = ID,
        priority = Priority,
        interval = Interval,
        ips = IPs
       }, State};
verify_vrrp(_Source, _Packet, State) ->
    {bad_packet, State}.

handle_packet({#vrrp_packet{id = Id} = Pkt, #state{mapping = D} = State}) ->
    case dict:find(Id, D) of
        {ok, {Pid, _MR}} ->
            vrrp_fsm:vrrp_msg(Pid, Pkt);
        error ->
            discard
    end,
    State;
handle_packet({bad_packet, State}) ->
    State.

send(#vrrp_packet{} = M, State) ->
    VrrpLen = ?VRRP_LEN + (length(M#vrrp_packet.ips) * 4),
    Len = ?V4_HEADER + VrrpLen,

    %% Convert IPs into a binary string...
    IPCount = length(M#vrrp_packet.ips),
    IPs = << <<A:8, B:8, C:8, D:8>> || {A, B, C, D} <- M#vrrp_packet.ips >>,

    SrcAddr = list_to_binary(tuple_to_list(State#state.v4_addr)),

    PseudoHeader =
        <<SrcAddr/binary,
          ?VRRP_V4_MCAST_BIN/binary,
          0:8,
          ?VRRP_PROTOCOL:8,
          VrrpLen:16/big-integer>>,

    IPHdr =
        <<4:4, 5:4,
          192:8,    %% TOS - 'Internet Control'
          Len:16/big-integer,
          0:32,     %% Fragment ID and offset
          ?VRRP_TTL:8,
          ?VRRP_PROTOCOL:8,
          0:16,
          SrcAddr/binary,
          ?VRRP_V4_MCAST_BIN/binary>>,

    Vrrp =
        <<3:4,      %% VRRP Version
          1:4,      %% Announcement (only type we know)
          (M#vrrp_packet.id):8/integer,
          (M#vrrp_packet.priority):8/integer,
          IPCount:8/integer,
          0:4,
          (M#vrrp_packet.interval):12/big-integer>>,

    CSum = checksum_1([PseudoHeader, Vrrp, IPs]),
    R = gen_udp:send(State#state.v4_wsocket, ?VRRP_V4_MCAST, 0, [IPHdr, Vrrp, <<CSum:16/big-integer>>, IPs]),
    {R, State}.

checksum_1(BinList) when is_list(BinList) ->
    checksum_1(BinList, 0);
checksum_1(Bin) ->
    (bnot checksum_2(Bin, 0)) band ?INT16MAX.

checksum_1([], Csum) ->
    (bnot Csum) band ?INT16MAX;
checksum_1([Bin|T], Csum) ->
    checksum_1(T, checksum_2(Bin, Csum)).

checksum_2(Bin = <<N1:16/integer, N2:16/integer, N3:16/integer,
		  N4:16/integer, Rem/binary>>, Csum) when size(Bin) >= 8 ->
    checksum_2(Rem, Csum+N1+N2+N3+N4);
checksum_2(<<Num:16/integer, Remainder/binary>>, Csum) ->
    checksum_2(Remainder, Csum + Num);
checksum_2(<<Num:8/integer>>, Csum) ->
    checksum_2(<<>>, Csum+(Num bsl 8));
checksum_2(<<>>, Csum) when Csum > ?INT16MAX ->
    Carry = Csum bsr 16,
    checksum_2(<<>>, (Csum band ?INT16MAX) + Carry);
checksum_2(<<>>, Csum) ->
    Csum.
