-module(ecnty_increment_fsm).
-compile([{parse_transform, lager_transform}]).

-behaviour(gen_fsm).
-export([code_change/4,
         handle_event/3,
         handle_info/3,
         handle_sync_event/4,
         init/1,
         terminate/3]).

-export([start_link/5]).

-export([increment/4]).

-export([prepare/2, waiting_local/2, waiting_remote/2, execute_local/2]).

-record(state, {from, key, value, remote_vnodes, local_vnode, total_responses = 0, num_w = 0, n = 0, w = 0}).

increment(Key, Value, N, W) ->
  ecnty_increment_fsm_sup:start_fsm(node(), [self(), Key, Value, N, W]).

start_link(From, Key, Value, N, W) ->
  gen_fsm:start_link(?MODULE, [From, Key, Value, N, W], []).

init([From, Key, Value, N, W]) ->
  lager:debug("init ~p ~n", [Key]),
  State = #state{from = From, key = Key, value = Value, n = N, w = W},
  {ok, prepare, State, 0}.

prepare(timeout, State=#state{key = Key, from = From, value = Value, n = N, w = W}) ->
  lager:debug("Prepare ~p", [Key]),
  DocIdx = riak_core_util:chash_key({<<"">>, list_to_binary(Key)}),
  Preflist = riak_core_apl:get_apl(DocIdx, N, ecnty),
  lager:debug("Preflist ~p", [Preflist]),
  {LocalNodes, OtherNodes} = lists:partition(fun({_Index, Node}) -> Node =:= node() end, Preflist),
  lager:debug("Local Node ~p OtherNodes ~p", [LocalNodes, OtherNodes]),
  case LocalNodes of
    [] -> 
      RemoteNode = element(2, hd(OtherNodes)),
      case ecnty_increment_fsm_sup:start_fsm(RemoteNode, [From, Key, Value, N, W]) of
        {ok, _Pid} ->
          {stop, normal, State};
        {error, Reason} ->
          send_error({failed_to_start_coordinator, Reason}, State)
      end;
     [FirstNode|OtherLocalNodes] ->
       NewState = State#state{remote_vnodes = OtherLocalNodes ++ OtherNodes,
                              local_vnode = FirstNode},
       {next_state, execute_local, NewState, 0}
  end.

execute_local(timeout, State=#state{key = Key, value = Value, local_vnode = LocalVNode}) ->
  lager:debug("Execute Local ~p ~n", [Key]),
  ecnty_vnode:increment(LocalVNode, Key, Value, {fsm, undefined, self()}),
  {next_state, waiting_local, State}.


  
waiting_local({ok, NewCounters}, State=#state{key = Key, remote_vnodes = RemoteVNodes, w = W}) ->
  lager:debug("Waiting local ~p ~n", [Key, RemoteVNodes, length(RemoteVNodes)]),
  
  NewState = State#state{num_w = 1, total_responses = 1},
  
  case W =:= 1 of
    true ->
      send_success(NewState);
    _ ->
      ecnty_vnode:merge(RemoteVNodes, Key, NewCounters, {fsm, undefined, self()}),
      lager:debug("Executed on Nodes ~p", [RemoteVNodes]),
      {next_state, waiting_remote, NewState}
  end;
  
waiting_local(_Reason, State) ->
  send_error(local_write_failed, State).

process_reply(ok, #state{total_responses = TotalResponses, num_w = NumW} = State) ->
  State#state{num_w = NumW + 1, total_responses = TotalResponses + 1};
process_reply(_Reason, #state{total_responses = TotalResponses} = State) ->
  State#state{total_responses = TotalResponses + 1}.
  
check_error(#state{total_responses = TotalResponses, n = N} = State) ->
  case TotalResponses =:= N of
    true ->
      send_error(write_threshold_not_met, State);
    _ ->
      {next_state, waiting, State}
  end.
  
waiting_remote(Reply, State) ->
  lager:debug("Waiting remote ~n", []),
  NewState = process_reply(Reply, State),
  case NewState#state.num_w =:= NewState#state.w of
    true -> 
      send_success(NewState);
    _ ->
      check_error(NewState) 
  end.

send_success(State = #state{from = From}) ->
  From ! ok,
  {stop, normal, State}.
  
send_error(Error, State=#state{from = From}) ->
  From ! {error, Error},
  {stop, normal, State}.

handle_info(_Info, _StateName, State) ->
  {stop, badmsg, State}.

handle_event(_Event, _StateName, State) ->
  {stop, badmsg, State}.

handle_sync_event(_Event, _From, _StateName, State) ->
  {stop, badmsg, State}.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

terminate(_Reason, _SN, _SD) ->
  ok.
 
