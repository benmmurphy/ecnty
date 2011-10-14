-module(simple_vclock).
-export([update/1, merge/2, new/0, new_obj/1, update_obj/2, obj/1, merge/1]).

new() ->
  0.

obj({_Clock, Object}) ->
  Object.

update(Clock) ->
  Clock + 1.

new_obj(Object) ->
  {new(), Object}.

update_obj({Clock, _Object}, NewObject) ->
  {update(Clock), NewObject}.

merge(List) ->
  lists:foldl(fun merge/2, hd(List), tl(List)).

merge({ClockLhs, Lhs}, {ClockRhs, _Rhs}) when ClockLhs > ClockRhs ->
  {ClockLhs, Lhs};
merge(_, {ClockRhs, Rhs}) ->
  {ClockRhs, Rhs}.
  
