%%%-------------------------------------------------------------------
%%% @author Rick  Payne <rickp@OtoloNetworks.com>
%%% @copyright (C) 2017, Rick Payne
%%% @doc
%%%
%%% The handler for the VRRP protocol - one per physical interface.
%%% This demultiplexes the incoming VRRP messages and sends them to
%%% the registered FSM handler for the ID.
%%%
%%% @end
%%% Created :  7 Aug 2017 by Rick Payne <rickp@OtoloNetworks.com>
%%%-------------------------------------------------------------------
-module(vrrp_interface_manager).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([set/3, remove/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          mapping :: dict:dict()
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
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
set(Interface, Id, FSMPid) ->
    gen_server:call(?SERVER, {set_mapping, Interface, Id, FSMPid}).

remove(Interface, Id) ->
    gen_server:call(?SERVER, {remove_mapping, Interface, Id}).

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
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{mapping = dict:new()}}.

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
handle_call({set_mapping, Interface, Id, FSMPid}, _From, #state{mapping = D} = State) ->
    {InterfacePid, NewState} =
        case dict:find(Interface, D) of
            error ->
                start_new_interface(Interface, Id, FSMPid, State);
            {ok, {P, _MR}} ->
                vrrp_interface:add_mapping(P, Id, FSMPid),
                {p, State}
        end,
    {reply, {ok, InterfacePid}, NewState};
handle_call({remove_mapping, Interface, Id}, _From, #state{mapping = D} = State) ->
    NewState =
        case dict:find(Interface, D) of
            error ->
                %% No-op, or race??
                State;
            {ok, {P, _MR}} ->
                vrrp_interface:remove_mapping(P, Id),
                State
        end,
    {reply, ok, NewState};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_info(Info, State) ->
    io:format("vrrp_interface_manager: received ~p~n", [Info]),
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
start_new_interface(Interface, Id, Pid, #state{mapping = D} = State) ->
    {ok, NewPid} = vrrp_interface:start_link([{interface, Interface}]),
    MonitorRef = erlang:monitor(process, NewPid),
    vrrp_interface:add_mapping(NewPid, Id, Pid),
    {NewPid, State#state{mapping = dict:store(Interface, {NewPid, MonitorRef}, D)}}.
    
    
