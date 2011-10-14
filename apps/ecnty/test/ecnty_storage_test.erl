-module(ecnty_storage_test).
-include_lib("eunit/include/eunit.hrl").
-export([storage_tests/2]).

bind(Fun, Param1) ->
    fun() -> Fun(Param1) end.

storage_tests(Module, ModuleSetupFun) ->
     {setup,
      fun() -> ModuleSetupFun(), Module end,
      [bind(F, Module) || F <- [
          fun simple_put_get_test/1,
          fun fold_objects_test/1,
          fun drop_test/1]]
     }.

simple_put_get_test({Module, Config}) ->
  {ok, XState1} = Module:start(456, Config),
  ?assertEqual(true, Module:is_empty(XState1)),
  ?assertEqual({error, not_found}, Module:get(XState1, "foo")),
  {ok, XState2} = Module:put(XState1, "key1", "value1"),
  {Result, Value} = Module:get(XState2, "key1"),
  ?assertEqual(ok, Result),
  ?assertEqual("value1", Value),
  ?assertEqual(false, Module:is_empty(XState2)),
  Module:stop(XState2).

create_with_two_keys({Module, Config}, Partition) ->
  {ok, XState1} = Module:start(Partition, Config),
  {ok, XState2} = Module:put(XState1, "key1", "value1"),
  Module:put(XState2, "key2", "value2").

fold_objects_test({Module, Config} = MC) ->
  {ok, XState3} = create_with_two_keys(MC, 456),
  {ok, Acc} = Module:fold_objects(XState3, fun(K, V, Acc) -> [{K,V}|Acc] end, []),
  ?assertEqual([{"key1", "value1"}, {"key2", "value2"}], lists:sort(Acc)).

drop_test({Module, Config} = MC) ->
  {ok, XState1} = create_with_two_keys(MC, 789),
  {ok, XState2} = Module:drop(XState1),
  ?assertEqual({error, not_found}, Module:get(XState2, "key1")),
  ?assertEqual({error, not_found}, Module:get(XState2, "key2")).
  
