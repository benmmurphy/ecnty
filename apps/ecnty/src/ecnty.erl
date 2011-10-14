-module(ecnty).
-include("ecnty.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-define(TIMEOUT, 5000).
-define(N, 3).
-define(R, 2).
-define(W, 2).

-export([
         ping/0,get/1,increment/2,get/3,increment/4
        ]).

%% Public API

% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, ecnty),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, ecnty_vnode_master).

get(Key) ->
    get(Key, ?N, ?R).

get(Key, N, R) ->
    ecnty_get_fsm:get(Key, N, R),
    Result = wait_for_result(?TIMEOUT),
    case Result of 
        {ok, Counters} ->
            {ok, partitioned_counter:value(partitioned_counter:merge(Counters))};
        Error ->
            Error
    end.

increment(Key, Val) ->
    increment(Key, Val, ?N, ?W).

increment(Key, Val, N, W) ->
    ecnty_increment_fsm:increment(Key, Val, N, W),
    wait_for_result(?TIMEOUT).

wait_for_result(Timeout) ->
    receive
        Result -> Result
    after Timeout ->
        {error, timeout}
    end.
