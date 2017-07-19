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

%% API
-export([start_link/1]).

-export([add_mapping/3, remove_mapping/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          interface,   %% Physical interface we are running on
          port,
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
    {ok, S} = procket:open(0, [{protocol, 16#112}, {type, raw}, {family, packet},
                               {interface, Interface}]),
    Port = erlang:open_port({fd, S, S}, [binary, stream]),
    {ok, #state{
            interface = Interface,
            port = Port,
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
           #state{mapping = D} = State) ->
    case dict:find(Id, D) of
        {ok, {_P, MR}} ->
            erlang:demonitor(MR);
        _ ->
            ok
    end,
    Ref = erlang:monitor(process, FSMPid),
    {reply, ok, State#state{mapping = dict:store(Id, {FSMPid, Ref}, D)}};
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
    {reply, ok, State#state{mapping = ND}}.

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
