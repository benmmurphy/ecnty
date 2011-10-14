-module(ecnty_storage_memory).
-behaviour(ecnty_storage).
-export([start/2, stop/1, put/3, get/2, drop/1, fold_objects/3, is_empty/1]).

-record(state, {dict = dict:new()}).


start(_Partition, _Config) ->
    {ok, #state{}}.


stop(_State) ->
    ok. 

get(#state{dict = Dict}, Key) ->
    case dict:find(Key, Dict) of 
        error -> {error, not_found};
        {ok, Value} -> {ok, Value}
    end.

put(#state{dict = Dict}=State, Key, Value) ->
    NewState = State#state{dict = dict:store(Key, Value, Dict)},
    {ok, NewState}.

is_empty(#state{dict = Dict}) ->
    dict:size(Dict) =:= 0.

drop(State) ->
    {ok, State#state{dict = dict:new()}}.

fold_objects(#state{dict = Dict}, FoldFun, Acc) ->
    {ok, dict:fold(FoldFun, Acc, Dict)}.

