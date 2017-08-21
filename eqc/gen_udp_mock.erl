%%% File        : gen_udp_mock.erl
%%% Author      : Ulf Norell
%%% Description :
%%% Created     : 21 Aug 2017 by Ulf Norell
-module(gen_udp_mock).

-compile([export_all, nowarn_export_all]).

-include("packets.hrl").

-define(VRRP_PROTOCOL, 112).

send(Socket, Addr, 0, Data) ->
  catch gen_udp:send(Socket, Addr, parse_packet(Data)).

recv(Pid, Socket, Ip, Port, Pkt) ->
  Pid ! {udp, Socket, Ip, Port, build_packet(Pkt)},
  ok.

%% -- Parsing packets --------------------------------------------------------

parse_packet(Data) when is_list(Data) ->
  parse_packet(binary:list_to_bin(Data));
parse_packet(Data) ->
  << Version   : 4,
     _IHL      : 4,
     _DSCP     : 6,
     _ECN      : 2,
     _Len       : 16, %% TODO: check
     _Id        : 16,
     _Flags     : 3,
     _FOffs    : 13,
     TTL       : 8,
     Protocol  : 8,
     _Checksum : 16,
     Source    : 4 / binary,
     Dest      : 4 / binary,
     Rest / binary >> = Data,
  Packet =
    case Protocol of
      ?VRRP_PROTOCOL when TTL /= 255 -> bad;
      ?VRRP_PROTOCOL                 -> parse_vrrp(Source, Dest, Rest);
      _                              -> {Protocol, Rest}
    end,
  Vsn = case Version of
          4 -> ipv4;
          6 -> ipv6
        end,
  #pkt{ src = parse_ip(Source), dst = parse_ip(Dest), version = Vsn, packet = Packet }.

parse_vrrp(Src, Dst, Data) ->
  try
    << 3           : 4,
       1           : 4,
       VRId        : 8,
       Priority    : 8,
       IPCount     : 8,
       0           : 4,
       MaxAdverInt : 12,
       _Checksum   : 16,
       Rest / binary >> = Data,
    check_vrrp_checksum(Src, Dst, Data),
    IPs = [ parse_ip(Bin) || <<Bin:4/binary>> <= Rest ],
    IPCount = length(IPs),
    #vrrp_packet
    { id            = VRId
    , priority      = Priority
    , max_adver_int = MaxAdverInt
    , ips           = IPs }
  catch
    _:bad_checksum -> {vrrp_checksum_fail, Data};
    _:_            -> {bad_vrrp_packet, Data}
  end.

check_vrrp_checksum(Src, Dst, Data) ->
  case compute_vrrp_checksum(Src, Dst, Data) of
    0 -> ok;
    _ -> error(bad_checksum)
  end.

parse_ip(Bin) -> list_to_tuple(binary_to_list(Bin)).

%% -- Building packets -------------------------------------------------------

build_packet(#pkt{ version = Version, src = Src, dst = Dst, packet = Vrrp }) ->
  TTL = 255,
  SrcBin = build_ip(Src),
  DstBin = build_ip(Dst),
  Data = build_vrrp(SrcBin, DstBin, Vrrp),
  Len = byte_size(Data) + 20,
  set_ip_checksum(
  <<Version        : 4,
    5              : 4,
    192            : 8,
    Len            : 16,
    0              : 16,
    0              : 3,
    0              : 13,
    TTL            : 8,
    ?VRRP_PROTOCOL : 8,
    0              : 16,
    SrcBin         : 4 / binary,
    DstBin         : 4 / binary,
    Data               / binary>>).

build_vrrp(Src, Dst, #vrrp_packet{ id = Id, priority = Prio, max_adver_int = Int, ips = Ips }) ->
  IpsData = << <<(build_ip(Ip))/binary>> || Ip <- Ips >>,
  IpCount = length(Ips),
  set_vrrp_checksum(Src, Dst,
    <<3       : 4,
      1       : 4,
      Id      : 8,
      Prio    : 8,
      IpCount : 8,
      0       : 4,
      Int     : 12,
      0       : 16,
      IpsData / binary>>).

build_ip({A, B, C, D}) ->
  <<A, B, C, D>>.

set_ip_checksum(Bin = <<Pre:10/binary, 0:16, Post/binary>>) ->
  Checksum = compute_ip_checksum(Bin),
  <<Pre/binary, Checksum:16, Post/binary>>.

compute_ip_checksum(<<Hd:20/binary, _/binary>>) ->
  checksum(Hd).

set_vrrp_checksum(Src, Dst, Bin = <<Pre:6/binary, 0:16, Post/binary>>) ->
  Checksum = compute_vrrp_checksum(Src, Dst, Bin),
  <<Pre/binary, Checksum:16, Post/binary>>.

compute_vrrp_checksum(Src, Dst, Bin) ->
  Len = byte_size(Bin),
  PseudoHd = <<Src/binary, Dst/binary, 0:8, ?VRRP_PROTOCOL:8, Len:16>>,
  checksum(<<PseudoHd/binary, Bin/binary>>).

checksum(Bin) ->
  Bin1 = if byte_size(Bin) rem 2 == 0 -> Bin;
            true -> <<Bin/binary, 0>>
         end,
  bnot carry(lists:sum([ W || <<W:16/big>> <= Bin1 ])) band 16#FFFF.

carry(N) when N =< 16#FFFF -> N;
carry(N)                   -> carry(N bsr 16 + N band 16#FFFF).

