%%%-------------------------------------------------------------------
%%% @author Rick  Payne <rickp@OtoloNetworks.com>
%%% @copyright (C) 2017, Rick Payne
%%% @doc
%%%
%%% @end
%%% Created : 11 Aug 2017 by Rick Payne <rickp@OtoloNetorks.com>
%%%-------------------------------------------------------------------
-module(vrrp_socket).

%% API
-export([init/0]).

%% NIF supplied funcs
-export ([create_socket/2]).

-on_load(init/0).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    erlang:load_nif(code:priv_dir(vrrp) ++ "/vrrp_socket", 0).

create_socket(Family, Interface) ->
    "NIF library missing".
