%% this is stolen from riak_kv_vnode.erl -- all bugs are mine :)

-module(vnode_id).
-export([get_vnodeid/1, clear_vnodeid/1]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Get the vnodeid, assigning and storing if necessary
get_vnodeid(Index) ->
    F = fun(Status) ->
                case proplists:get_value(vnodeid, Status, undefined) of
                    undefined ->
                        assign_vnodeid(os:timestamp(), 
                                       riak_core_nodeid:get(),
                                       Status);
                    VnodeId ->
                        {VnodeId, Status}
                end
        end,
    update_vnode_status(F, Index). % Returns {ok, VnodeId} | {error, Reason}

%% Assign a unique vnodeid, making sure the timestamp is unique by incrementing
%% into the future if necessary.
assign_vnodeid(Now, NodeId, Status) ->
    {Mega, Sec, _Micro} = Now,
    NowEpoch = 1000000*Mega + Sec,
    LastVnodeEpoch = proplists:get_value(last_epoch, Status, 0),
    VnodeEpoch = erlang:max(NowEpoch, LastVnodeEpoch+1),
    VnodeId = <<NodeId/binary, VnodeEpoch:32/integer>>,
    UpdStatus = [{vnodeid, VnodeId}, {last_epoch, VnodeEpoch} | 
                 proplists:delete(vnodeid, 
                   proplists:delete(last_epoch, Status))],
    {VnodeId, UpdStatus}.

%% Clear the vnodeid - returns {ok, cleared}
clear_vnodeid(Index) ->
    F = fun(Status) ->
                {cleared, proplists:delete(vnodeid, Status)}
        end,
    update_vnode_status(F, Index). % Returns {ok, VnodeId} | {error, Reason}

update_vnode_status(F, Index) ->
    VnodeFile = vnode_status_filename(Index),
    ok = filelib:ensure_dir(VnodeFile),
    case read_vnode_status(VnodeFile) of
        {ok, Status} ->
            update_vnode_status2(F, Status, VnodeFile);
        {error, enoent} ->
            update_vnode_status2(F, [], VnodeFile);
        ER ->
            ER
    end.

update_vnode_status2(F, Status, VnodeFile) ->
    case F(Status) of
        {Ret, Status} -> % No change
            {ok, Ret};
        {Ret, UpdStatus} ->
            case write_vnode_status(UpdStatus, VnodeFile) of
                ok ->
                    {ok, Ret};
                ER ->
                    ER
            end
    end.
 
vnode_status_filename(Index) ->
    P_DataDir = app_helper:get_env(ecnty, platform_data_dir),
    VnodeStatusDir = app_helper:get_env(ecnty, vnode_status,
                                        filename:join(P_DataDir, "ecnty_vnode")),
    filename:join(VnodeStatusDir, integer_to_list(Index)).
    
read_vnode_status(File) ->
    case file:consult(File) of
        {ok, [Status]} when is_list(Status) ->
            {ok, proplists:delete(version, Status)};
        ER ->
            ER
    end.

write_vnode_status(Status, File) ->
    VersionedStatus = [{version, 1} | proplists:delete(version, Status)],
    TmpFile = File ++ "~",
    case file:write_file(TmpFile, io_lib:format("~p.", [VersionedStatus])) of
        ok ->
            file:rename(TmpFile, File);
        ER ->
            ER
    end.

-ifdef(TEST).

%% Check assigning a vnodeid twice in the same second
assign_vnodeid_restart_same_ts_test() ->
    Now1 = {1314,224520,343446}, %% TS=1314224520
    Now2 = {1314,224520,345865}, %% as unsigned net-order int <<78,85,121,136>>
    NodeId = <<1, 2, 3, 4>>,
    {Vid1, Status1} = assign_vnodeid(Now1, NodeId, []),
    ?assertEqual(<<1, 2, 3, 4, 78, 85, 121, 136>>, Vid1),
    %% Simulate clear
    Status2 = proplists:delete(vnodeid, Status1),
    %% Reassign
    {Vid2, _Status3} = assign_vnodeid(Now2, NodeId, Status2),
    ?assertEqual(<<1, 2, 3, 4, 78, 85, 121, 137>>, Vid2).

%% Check assigning a vnodeid with a later date
assign_vnodeid_restart_later_ts_test() ->
    Now1 = {1000,000000,0}, %% <<59,154,202,0>>
    Now2 = {2000,000000,0}, %% <<119,53,148,0>>
    NodeId = <<1, 2, 3, 4>>,
    {Vid1, Status1} = assign_vnodeid(Now1, NodeId, []),
    ?assertEqual(<<1, 2, 3, 4, 59,154,202,0>>, Vid1),
    %% Simulate clear
    Status2 = proplists:delete(vnodeid, Status1),
    %% Reassign
    {Vid2, _Status3} = assign_vnodeid(Now2, NodeId, Status2),
    ?assertEqual(<<1, 2, 3, 4, 119,53,148,0>>, Vid2).

%% Check assigning a vnodeid with a later date - just in case of clock skew
assign_vnodeid_restart_earlier_ts_test() ->
    Now1 = {2000,000000,0}, %% <<119,53,148,0>>
    Now2 = {1000,000000,0}, %% <<59,154,202,0>>
    NodeId = <<1, 2, 3, 4>>,
    {Vid1, Status1} = assign_vnodeid(Now1, NodeId, []),
    ?assertEqual(<<1, 2, 3, 4, 119,53,148,0>>, Vid1),
    %% Simulate clear
    Status2 = proplists:delete(vnodeid, Status1),
    %% Reassign
    %% Should be greater than last offered - which is the 2mil timestamp
    {Vid2, _Status3} = assign_vnodeid(Now2, NodeId, Status2),
    ?assertEqual(<<1, 2, 3, 4, 119,53,148,1>>, Vid2).
        
%% Test 
vnode_status_test_() ->
    {setup,
     fun() ->
             os:cmd("chmod u+rwx ecnty_vnode_status_test"),
             os:cmd("rm -rf ecnty_vnode_status_test"),
             application:set_env(ecnty, vnode_status, "ecnty_vnode_status_test"),
             ok
     end,
     fun(_) ->
             application:unset_env(ecnty, vnode_status),
             ?cmd("chmod u+rwx ecnty_vnode_status_test"),
             ?cmd("rm -rf ecnty_vnode_status_test"),
             ok
     end,
     [?_test(begin % initial create failure
                 ?cmd("rm -rf ecnty_vnode_status_test || true"),
                 ?cmd("mkdir ecnty_vnode_status_test"),
                 ?cmd("chmod -w ecnty_vnode_status_test"),
                 F = fun([]) ->
                             {shouldfail, [badperm]}
                     end,
                 Index = 0,
                 ?assertEqual({error, eacces},  update_vnode_status(F, Index))
             end),
      ?_test(begin % create successfully
                 ?cmd("chmod +w ecnty_vnode_status_test"),

                 F = fun([]) ->
                             {created, [created]}
                     end,
                 Index = 0,
                 ?assertEqual({ok, created}, update_vnode_status(F, Index))
             end),
      ?_test(begin % update successfully
                 F = fun([created]) ->
                             {updated, [updated]}
                     end,
                 Index = 0,
                 ?assertEqual({ok, updated}, update_vnode_status(F, Index))
             end),
      ?_test(begin % update failure
                 ?cmd("chmod 000 ecnty_vnode_status_test/0"),
                 ?cmd("chmod 500 ecnty_vnode_status_test"),
                 F = fun([updated]) ->
                             {shouldfail, [updatedagain]}
                     end,
                 Index = 0,
                 ?assertEqual({error, eacces},  update_vnode_status(F, Index))
            end)

     ]}.

-endif.
