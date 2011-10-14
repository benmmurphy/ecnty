-module(partitioned_counter).
-export([new/2, update/3, value/1, merge/1]).

value(Counters) ->
  lists:sum([simple_vclock:obj(Obj) || {_Id, Obj} <-  Counters]).

new(Id, InitialValue) ->
  [{Id, simple_vclock:new_obj(InitialValue)}].

update(Id, Value, Counters) ->

  case lists:keyfind(Id, 1, Counters) of 
    {Id, Obj} -> 
      NewObj = simple_vclock:update_obj(Obj, simple_vclock:obj(Obj) + Value),
      lists:keyreplace(Id, 1, Counters, {Id, NewObj});
    _ ->
      orddict:from_list([{Id, simple_vclock:new_obj(Value)}|Counters])
  end.


mergeTwoPartitions(Lhs, Rhs) ->
  orddict:merge(fun(_Id, Obj1, Obj2) ->
                    simple_vclock:merge(Obj1, Obj2)
                end,
                Lhs,
                Rhs).


merge(ListOfCounters) ->
  lists:foldl(fun mergeTwoPartitions/2, [], lists:map(fun orddict:from_list/1, ListOfCounters)).

  
