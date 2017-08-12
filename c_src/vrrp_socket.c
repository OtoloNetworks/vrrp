/*
 * vrrp_socket.cc
 *
 * Create the raw sockets bound to the appropriate device for VRRP.
 * Do it as a NIF rather than using the 'procket' port method.
 *
 * Rick Payne <rickp@otolonetworks.com>
 * August 2017
 */
#include <string.h>
#include <sys/cdefs.h>

#include <sys/ioctl.h>
#include <linux/in.h>
#include <net/if.h>

#include "erl_nif.h"

#define VRRP_PROTO 112  /* VRRP Protocol number */
#define VRRP_GROUP 0xe0000012

static ERL_NIF_TERM atom_ok, atom_missing, atom_error;

/* static int */
/* map_interface_to_ifindex(char *name) */
/* { */
/*   return 1; */
/* } */

static ERL_NIF_TERM
create_socket(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
  int rsock = -1, wsock = -1;
  int ifindex, on = 1, off = 1;
  struct ifreq ifr;
  struct ip_mreqn imr;
  char if_name[IFNAMSIZ];

  if (argc != 2) return atom_error;

  enif_get_string(env, argv[1], if_name, IFNAMSIZ, ERL_NIF_LATIN1);

  // Allocate a raw socket - requires root...
  rsock = socket(AF_INET, SOCK_RAW, VRRP_PROTO);
  if (rsock < 0) return atom_error;

  // printf("rsock: %d\nif_name: %s\n", rsock, if_name);
  // Map name to ifindex
  memset(&ifr, 0, sizeof(ifr));
  strncpy(ifr.ifr_name, if_name, sizeof(ifr.ifr_name));
  if (ioctl(rsock, SIOCGIFINDEX, &ifr) == -1) {
    return atom_missing;
  }
  ifindex = ifr.ifr_ifindex;

  // Add group membership
  memset(&imr, 0, sizeof(imr));
  imr.imr_multiaddr.s_addr = htonl(VRRP_GROUP);
  imr.imr_ifindex = ifindex;
  setsockopt(rsock, IPPROTO_IP, IP_ADD_MEMBERSHIP,
             (char *) &imr, (socklen_t)sizeof(struct ip_mreqn));

  // Now bind to device...
  setsockopt(rsock, SOL_SOCKET, SO_BINDTODEVICE,
             if_name, IFNAMSIZ);

  wsock = socket(AF_INET, SOCK_RAW, VRRP_PROTO);
  
  setsockopt(wsock, IPPROTO_IP, IP_MULTICAST_ALL, &off, sizeof(off));
  setsockopt(wsock, IPPROTO_IP, IP_HDRINCL, &on, sizeof(on));
  setsockopt(wsock, SOL_SOCKET, SO_BINDTODEVICE, if_name, IFNAMSIZ);
  memset(&imr, 0, sizeof(imr));
  imr.imr_ifindex = ifindex;
  setsockopt(wsock, IPPROTO_IP, IP_MULTICAST_IF, &imr, sizeof(imr));
  
  return enif_make_tuple2(env,
                          enif_make_int(env, rsock),
                          enif_make_int(env, wsock));
}

static int
load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    atom_ok = enif_make_atom(env,"ok");
    atom_missing = enif_make_atom(env, "missing");
    atom_error = enif_make_atom(env, "error");

    return 0;
}

static void
unload(ErlNifEnv* env, void *priv_data)
{
  return;
}


static ErlNifFunc nif_funcs[] =
  {
    {"create_socket", 2, create_socket}
  };

ERL_NIF_INIT(vrrp_socket,nif_funcs,load,NULL,NULL,unload)

