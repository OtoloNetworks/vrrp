%%%-------------------------------------------------------------------
%%% @author Rick Payne <rickp@OtoloNetworks.com>
%%% @copyright (C) 2017, Rick Payne
%%% @doc
%%%
%%% @end
%%% Created : 18 Sep 2017 by Rick Payne <rickp@OtoloNetworks.com1>
%%%-------------------------------------------------------------------
-module(vrrp_example).

%% API
-export([become_master/4, become_backup/4]).

%%%===================================================================
%%% API
%%%===================================================================
become_master(_Family, Interface, Id, _VIPs) ->
    io:format("Becoming master for VRRP instance ~p on ~s~n", [Id, Interface]).

become_backup(_Family, Interface, Id, _VIPs) ->
    io:format("Becoming slave for VRRP instance ~p on ~s~n", [Id, Interface]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
