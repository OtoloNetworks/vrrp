
-record(pkt, { version, src, dst, packet }).

-record(vrrp_packet,
  { id
  , priority
  , max_adver_int
  , ips = [] }).
