-module(ecnty_vnode_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

setup() ->
    application:set_env(ecnty, storage_module, ecnty_storage_memory),
    application:set_env(ecnty, vnode_status, "kv_vnode_status_test").
    
increment_test() ->
  setup(),
  {ok, State1} = ecnty_vnode:init([123]),
  {reply, {ok, Counters}, State2} = ecnty_vnode:handle_command({increment, "Key1", 4}, undefined, State1),
  ?assertEqual(4, partitioned_counter:value(Counters)), 
  {reply, {ok, Counters2}, State3} = ecnty_vnode:handle_command({increment, "Key2", 5}, undefined, State2),
  ?assertEqual(5, partitioned_counter:value(Counters2)),
  {reply, {ok, Counters3}, State4} = ecnty_vnode:handle_command({increment, "Key1", 6}, undefined, State3),
  ?assertEqual(10, partitioned_counter:value(Counters3)),
  {reply, {r, {ok, Counters4}, 123}, State5} = ecnty_vnode:handle_command({get, "Key1"}, undefined, State4),
  ?assertEqual(10, partitioned_counter:value(Counters4)),
  ?assertEqual(ok, ecnty_vnode:terminate(noreason, State5)).

create_counter() ->
  Pc1 = partitioned_counter:new(<<"id1">>, 5),
  Pc2 = partitioned_counter:new(<<"id2">>, 6),
  Pc3 = partitioned_counter:merge([Pc1, Pc2]),
  Pc3.

merge_empty_test() ->
  setup(),
  {ok, From1} = ecnty_vnode:init([234]),
  Pc3 = create_counter(),
  {reply, ok, From2} = ecnty_vnode:handle_command({merge, "key", Pc3}, undefined, From1),
  {reply, {r, {ok, Pc4}, 234}, From3} = ecnty_vnode:handle_command({get, "key"}, undefined, From2),
  ?assertEqual(Pc3, Pc4),
  ecnty_vnode:terminate(no_reason, From3).

merge_non_empty_test() ->
  setup(),
  {ok, From1} = ecnty_vnode:init([234]),
  {reply, {ok, Counters}, From2} = ecnty_vnode:handle_command({increment, "key", 4}, undefined, From1),
  Pc3 = create_counter(),
  {reply, ok, From3} = ecnty_vnode:handle_command({merge, "key", Pc3}, undefined, From2),
  {reply, {r, {ok, Pc4}, 234}, From4} = ecnty_vnode:handle_command({get, "key"}, undefined, From3),
  ?assertEqual(15, partitioned_counter:value(Pc4)),
  ecnty_vnode:terminate(no_reason, From4).

handoff_test() ->
  setup(),
  {ok, From1} = ecnty_vnode:init([234]),
  {ok, To1} = ecnty_vnode:init([456]),
  {reply, {ok, Counters}, From2} = ecnty_vnode:handle_command({increment, "Key1", 4}, undefined, From1),
  {reply, {ok, Counters2}, From3} = ecnty_vnode:handle_command({increment, "Key2", 5}, undefined, From2),
  {false, From4} = ecnty_vnode:is_empty(From3),
  {true,  From5} = ecnty_vnode:handoff_starting(undefined, From4),
  {forward, From6} = ecnty_vnode:handle_handoff_command({increment, "Key1", 1}, undefined, From5),
  FoldFun = fun(Key, Val, Acc) ->
              io:format("received ~p ~p ~n", [Key, Val]),
              Encoded = ecnty_vnode:encode_handoff_item(Key, Val),
              {reply, ok, NextTo} = ecnty_vnode:handle_handoff_data(Encoded, Acc),
              NextTo
            end,
  {reply, ToN1, From7} = ecnty_vnode:handle_handoff_command(?FOLD_REQ{foldfun = FoldFun, acc0=To1}, undefined, From6),
  {ok, From8} = ecnty_vnode:handoff_finished(undefined, From7),
  {ok, From9} = ecnty_vnode:delete(From8),
  ecnty_vnode:terminate(no_reason, From9),
  {reply, {r, {ok, Counters}, 456}, ToN2} = ecnty_vnode:handle_command({get, "Key1"}, undefined, ToN1),
  {reply, {r, {ok, Counters2}, 456}, ToN3} = ecnty_vnode:handle_command({get, "Key2"}, undefined, ToN2).

