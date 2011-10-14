-module(ecnty_vnode).
-behaviour(riak_core_vnode).
-include("ecnty.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").
-compile([{parse_transform, lager_transform}]).

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_exit/3,
         handle_coverage/4]).

-export([increment/4, get/3, merge/4]).

-define(MASTER, ecnty_vnode_master).
-define(sync(PrefList, Command, Master),
        riak_core_vnode_master:sync_command(PrefList, Command, Master)).


-record(state, {storage_module, storage_state, my_id = "" :: string(), partition}).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

get(Preflist, Key, Sender) ->
    riak_core_vnode_master:command(Preflist, {get, Key}, Sender, ?MASTER).

merge(Preflist, Key, Counters, Sender) ->
    riak_core_vnode_master:command(Preflist, {merge, Key, Counters}, Sender, ?MASTER).

increment(Preflist, Key, Val, Sender) ->
    riak_core_vnode_master:command(Preflist, {increment, Key, Val}, Sender, ?MASTER).

init([Partition]) ->
    Module = app_helper:get_env(ecnty, storage_module),
    Configuration = app_helper:get_env(ecnty),
        
    {ok, Vid} = vnode_id:get_vnodeid(Partition),
    {ok, StorageState} = Module:start(Partition, Configuration),
    {ok, #state { storage_module = Module, partition=Partition, my_id = Vid, storage_state = StorageState }}.


do_get(StatName, _Sender, #state{storage_module = StorageModule, storage_state=StorageState}=State) ->
      lager:debug("do_get ~p", [StatName]),
      case StorageModule:get(StorageState, StatName) of
        {ok, Counters} -> 
            lager:debug("do_get ok ~p", [StatName]),
            {reply, {ok, Counters}, State};
        {error, Reason} -> 
            lager:debug("do_get error ~p ~p", [StatName, Reason]),
            {reply, {error, Reason}, State}
      end.
 
do_increment(StatName, Value, _Sender, #state{storage_module = StorageModule, my_id = MyId, storage_state=StorageState}=State) ->

    case StorageModule:get(StorageState, StatName) of
      {ok, Counters} ->
        NewCounters = partitioned_counter:update(MyId, Value, Counters),
        store_update(NewCounters, StatName, State);
      {error, not_found} ->
        NewCounters = partitioned_counter:new(MyId, Value),
        store_update(NewCounters, StatName, State);
      {error, Reason} ->
        {reply, {error, Reason}, State}
    end.

do_merge_counters(StatName, CountersToMerge, #state{storage_module = StorageModule, storage_state=StorageState}=State) ->
    lager:debug("Do Merge Counters"),
    case StorageModule:get(StorageState, StatName) of
      {ok, Counters} ->
        NewCounters = partitioned_counter:merge([Counters, CountersToMerge]),
        store_merged_counters(NewCounters, StatName, State);
      {error, not_found} ->
        store_merged_counters(CountersToMerge, StatName, State);
      {error, Reason} ->
        {reply, {error, Reason}, State}
    end.
 
store_merged_counters(Counters, StatName, #state{storage_module = StorageModule, storage_state=StorageState}=State) ->
  case StorageModule:put(StorageState, StatName, Counters) of
     {ok, NewStorageState} -> 
         lager:debug("Counter Stored"),
         {reply, ok, State#state{storage_state=NewStorageState}};
     {error, Reason, NewStorageState} -> 
         {reply, {error, Reason}, State#state{storage_state=NewStorageState}}
  end.

store_update(Counters, StatName, #state{storage_module = StorageModule, storage_state=StorageState}=State) ->
  case StorageModule:put(StorageState, StatName, Counters) of
     {ok, NewStorageState} -> {reply, {ok, Counters}, State#state{storage_state=NewStorageState}};
     {error, Reason, NewStorageState} -> {reply, {error, Reason}, State#state{storage_state=NewStorageState}}
  end.

% Sample command: respond to a ping
handle_command(ping, _Sender, State) ->
    {reply, {pong, State#state.partition}, State};

handle_command({increment, StatName, Value},  Sender, State) ->
    do_increment(StatName, Value, Sender, State);

handle_command({merge, Key, Counters}, _Sender, State) ->
    do_merge_counters(Key, Counters, State); 

handle_command({get, StatName}, Sender, State) ->
    do_get(StatName, Sender, State);

handle_command(Message, _Sender, State) ->
    lager:error("Unhandled Command ~p", [Message]),
    {noreply, State}.

handle_handoff_command(?FOLD_REQ{foldfun=FoldFun, acc0=Acc0}, _Sender, #state{storage_module = StorageModule, storage_state=StorageState}=State) ->
    FoldResult = StorageModule:fold_objects(StorageState, FoldFun, Acc0),
    case FoldResult of
      {ok, Acc} ->
        {reply, Acc, State};
      {error, ERROR} ->
        {reply, ERROR, State}
    end;

handle_handoff_command(_Req, _Sender, State) -> {forward, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(Data, State) ->
    {CounterName, Counters} = binary_to_term(Data),
    do_merge_counters(CounterName, Counters, State).

encode_handoff_item(CounterName, CounterValue) ->
    term_to_binary({CounterName, CounterValue}).

is_empty(#state{storage_state=StorageState, storage_module = StorageModule}=State) ->
    {StorageModule:is_empty(StorageState), State}.

delete(#state{storage_module = StorageModule, storage_state=StorageState, partition=Partition}=State) ->
    {ok, cleared} = vnode_id:clear_vnodeid(Partition),
    case StorageModule:drop(StorageState) of 
      {ok, NewStorageState} ->
        ok;
      {error, _Reason, NewStorageState} ->
        ok
    end,

    {ok, State#state{storage_state=NewStorageState, my_id=undefined}}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.
        
terminate(_Reason, #state{storage_module = StorageModule, storage_state=StorageState}) ->
    StorageModule:stop(StorageState),
    ok.
    
    

