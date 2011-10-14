-module(ecnty_storage).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
  [{start, 2},
   {stop, 1},
   {get, 2},
   {put, 3},
   {drop, 1},
   {fold_objects, 3},
   {is_empty, 1}].


