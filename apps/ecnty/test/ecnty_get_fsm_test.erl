-module(ecnty_get_fsm_test).
-include_lib("eunit/include/eunit.hrl").
-compile([{parse_transform, lager_transform}]).

fsm_test_() ->
    {setup,
     fun setup/0,
     fun tear_down/1,
    [fun first_two_nodes_not_found_but_last_one_has_value_tst/0,
     fun not_found_tst/0,
     fun all_nodes_found_but_need_read_repair_tst/0]
    }.
    
deps() ->
    [sasl, riak_sysmon, webmachine, os_mon, riak_core].

start_deps(Deps) ->
    [ ok = application:start(Dep) || Dep <- Deps].

stop_deps(Deps) ->
    lists:map(fun application:stop/1, Deps).

tear_down(_) ->
    application:stop(ecnty),
    stop_deps(lists:reverse(deps())).

setup() ->
    lager:start(),
    application:set_env(riak_core, ring_creation_size, 64),
    application:set_env(riak_core, vnode_inactivity_timeout, infinity),
    application:set_env(ecnty, platform_data_dir, "data"),
    application:set_env(ecnty, storage_module, ecnty_storage_memory),

    start_deps(deps()),
    Ring = riak_core_ring:fresh(),
    riak_core_ring_manager:set_ring_global(Ring),
    application:start(ecnty).
        
receive_message(Ref) ->
    receive
        {Ref, Msg} -> Msg
    end.
    
all_nodes_found_but_need_read_repair_tst() ->
    Key = "all_nodes",
    Preflist = ecnty_get_fsm:get_preflist_for_key(Key, 3),
    
    {ok, Counters1} = ecnty_vnode:increment_sync(lists:nth(1, Preflist), Key, 1),
    {ok, Counters2} = ecnty_vnode:increment_sync(lists:nth(2, Preflist), Key, 1),
    {ok, Counters3} = ecnty_vnode:increment_sync(lists:nth(3, Preflist), Key, 1),
    
    
    CountersMerged = partitioned_counter:merge([Counters1, Counters2, Counters3]),
    
    
    ?assertEqual({ok, 2}, ecnty:get(Key, 3, 2)),
    
    timer:sleep(250),
    
    Gets = [ecnty_vnode:get_sync(IdxNode, Key) || IdxNode <- Preflist],
    
    
    ?assertMatch({r, {ok, CountersMerged}, _}, lists:nth(1, Gets)),
    ?assertMatch({r, {ok, CountersMerged}, _}, lists:nth(2, Gets)),
    ?assertMatch({r, {ok, CountersMerged}, _}, lists:nth(3, Gets)),
    
    ?assertEqual({ok, 3}, ecnty:get(Key, 3, 2)).
    
    
    
    
first_two_nodes_not_found_but_last_one_has_value_tst() ->
  
    Key = "two_one",
    Preflist = ecnty_get_fsm:get_preflist_for_key(Key, 3),
    LastNode = lists:last(Preflist),
    Ref = make_ref(),
    ecnty_vnode:increment([LastNode], Key, 1, {raw, Ref, self()}),
    
    {ok, Counters} = receive_message(Ref),
    ?assertEqual({ok, 1}, ecnty:get(Key, 3, 2)),
    
    Gets = [ecnty_vnode:get_sync(IdxNode, Key) || IdxNode <- Preflist],
    
    
    % check for read repair
    
    ?assertMatch({r, {ok, Counters}, _}, lists:nth(1, Gets)),
    ?assertMatch({r, {ok, Counters}, _}, lists:nth(2, Gets)),
    ?assertMatch({r, {ok, Counters}, _}, lists:nth(3, Gets)).

not_found_tst() ->
    Key = "not_found",
        
    ?assertEqual({ok, 0}, ecnty:get(Key, 3, 2)).
