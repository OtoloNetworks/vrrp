%%%-------------------------------------------------------------------
%% @doc vrrp public API
%% @end
%%%-------------------------------------------------------------------

-module(vrrp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    R = vrrp_sup:start_link(),
    start_all(application:get_env(vrrp, instance)),
    R.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
start_all(undefined) ->
    ok;
start_all({ok, Keys}) ->
    lists:foreach(
      fun({Id, Params}) ->
              vrrp_sup:start_vrrp([{id, Id}] ++ Params)
      end, Keys).

