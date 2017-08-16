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
#include <stdint.h>
#include <unistd.h>

#include <sys/ioctl.h>
#include <net/if.h>
#include <arpa/inet.h>

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
make_error(ErlNifEnv *env, char *string)
{
  return enif_make_tuple2(env,
                          atom_error,
                          enif_make_string(env, string, ERL_NIF_LATIN1));
}


static ERL_NIF_TERM
create_socket_ipv4(ErlNifEnv *env, char *if_name)
{
  int rsock = -1, wsock = -1;
  int ifindex, on = 1, off = 1;
  struct ifreq ifr;
  struct ip_mreqn imr;
  uint32_t addr;

  // Allocate a raw socket - requires root...
  rsock = socket(AF_INET, SOCK_RAW, VRRP_PROTO);
  if (rsock < 0) return make_error(env, "Failed to create raw socket");

  // Map name to ifindex
  memset(&ifr, 0, sizeof(ifr));
  strncpy(ifr.ifr_name, if_name, sizeof(ifr.ifr_name));
  if (ioctl(rsock, SIOCGIFINDEX, &ifr) == -1) {
    return make_error(env, "Failed to map interface name to ifindex");
  }
  ifindex = ifr.ifr_ifindex;

  // Get our address...
  if (ioctl(rsock, SIOCGIFADDR, (caddr_t) &ifr, NULL) == -1) {
    return make_error(env, "Failed to get our local address");
  }
  addr = ntohl(((struct sockaddr_in *) &(ifr.ifr_addr))->sin_addr.s_addr);

  // Add group membership
  memset(&imr, 0, sizeof(imr));
  imr.imr_multiaddr.s_addr = htonl(VRRP_GROUP);
  imr.imr_ifindex = ifindex;
  if (setsockopt(rsock, IPPROTO_IP, IP_ADD_MEMBERSHIP,
                 (char *) &imr, (socklen_t)sizeof(struct ip_mreqn)) != 0) {
    close(rsock);
    return make_error(env, "Failed to add VRRP group membership");
  }

  // Now bind to device...
  if (setsockopt(rsock, SOL_SOCKET, SO_BINDTODEVICE,
                 if_name, IFNAMSIZ) != 0) {
    close(rsock);
    return make_error(env, "Failed to bind to interface");
  }

  wsock = socket(AF_INET, SOCK_RAW, VRRP_PROTO);
  if (wsock < 0) {
    close(rsock);
    return make_error(env, "Failed to create write socket");
  }

  if (setsockopt(wsock, IPPROTO_IP, IP_MULTICAST_ALL, &off, sizeof(off)) != 0) {
    close(rsock);
    close(wsock);
    return make_error(env, "Failed to turn off all multicast");
  }

  if (setsockopt(wsock, IPPROTO_IP, IP_HDRINCL, &on, sizeof(on)) != 0) {
    close(rsock);
    close(wsock);
    return make_error(env, "Failed to turn HDRINCL on");
  }

  if (setsockopt(wsock, SOL_SOCKET, SO_BINDTODEVICE, if_name, IFNAMSIZ) != 0) {
    close(wsock);
    close(rsock);
    return make_error(env, "Failed to bind write socket to device");
  }
  
  memset(&imr, 0, sizeof(imr));
  imr.imr_ifindex = ifindex;
  if (setsockopt(wsock, IPPROTO_IP, IP_MULTICAST_IF, &imr, sizeof(imr)) != 0) {
    close(rsock);
    close(wsock);
    return make_error(env, "Failed to turn on multicast for write socket");
  }

  return enif_make_tuple3(env,
                          enif_make_tuple4(env,
                                           enif_make_int(env, (addr >> 24) & 255),
                                           enif_make_int(env, (addr >> 16) & 255),
                                           enif_make_int(env, (addr >> 8) & 255),
                                           enif_make_int(env, addr & 255)),
                          enif_make_int(env, rsock),
                          enif_make_int(env, wsock));
}

static ERL_NIF_TERM
create_socket(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
  char family[10];
  char if_name[IFNAMSIZ];

  if (argc != 2) return atom_error;

  if (enif_get_atom(env, argv[0], family, sizeof(family), ERL_NIF_LATIN1) == 0
      || enif_get_string(env, argv[1], if_name, IFNAMSIZ, ERL_NIF_LATIN1) == 0) {
    return error("argument error");
  }

  if (strcmp(family, "ipv4") == 0) {
    return create_socket_ipv4(env, if_name);
  } else if (strcmp(family, "ipv6") == 0) {
    return make_error(env, "IPv6 not yet supported");
  }
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

