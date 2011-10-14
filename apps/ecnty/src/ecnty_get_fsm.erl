-module(ecnty_get_fsm).
-compile([{parse_transform, lager_transform}]).

-behaviour(gen_fsm).
-export([code_change/4,
         handle_event/3,
         handle_info/3,
         handle_sync_event/4,
         init/1,
         terminate/3]).

-export([start_link/4]).

-export([get/3]).

-export([waiting/2]).

-record(state, {from, key, num_r = 0, total_responses = 0, result = [], r = 0, n = 0}).
-define(N, 3).
-define(R, 2).

get(Key, N, R) ->
  ecnty_get_fsm_sup:start_fsm(node(), [self(), Key, N, R]).

start_link(From, Key, N, R) ->
  gen_fsm:start_link(?MODULE, [From, Key, N, R], []).

init([From, Key, N, R]) ->
  lager:debug("get_fsm:init ~p", [Key]),
  State = #state{from = From, key = Key, r = R, n = N},
  DocIdx = riak_core_util:chash_key({<<"">>, list_to_binary(Key)}),
  Preflist = riak_core_apl:get_apl(DocIdx, N, ecnty),
  lager:debug("Preflist ~p", [Preflist]),
  ecnty_vnode:get(Preflist, Key, {fsm, undefined, self()}),
  {ok, waiting, State}.

process_reply({ok, Counters}, #state{num_r = NumR, total_responses = TotalResponses, result = Result} = State) ->
  lager:debug("Received counters ~p", [Counters]),
  State#state{num_r = NumR + 1, total_responses = TotalResponses + 1, result = [Counters | Result]};
process_reply(Error, #state{total_responses = TotalResponses} = State) ->
  lager:debug("Received error message from node ~p", [Error]),
  State#state{total_responses = TotalResponses + 1}.
    
waiting(Result, State=#state{from = From, r = R, n = N, key = Key}) ->
  NewState = process_reply(Result, State),
  case (NewState#state.num_r =:= R) or (NewState#state.total_responses =:= N) of
    true ->
      lager:debug("Finished ~p", [Key]),
      From ! {ok, NewState#state.result},
      {stop, normal, NewState};
    _    ->
      {next_state, waiting, NewState}
  end.

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
 
