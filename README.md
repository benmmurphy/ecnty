#Attributions
This is based on Ryan Zezeski try-try-try working blog. (https://github.com/rzezeski/try-try-try)

* i used his excellent templates (https://github.com/rzezeski/rebar_riak_core)
* the counter is quite similar but the way it is updated is slightly different. (https://github.com/rzezeski/try-try-try/tree/master/2011/riak-core-conflict-resolution)

Code used in the project has also been ripped from riak-kv from Basho (https://github.com/basho/riak_kv)

* vnode_id.erl is copy and pasted from riak_kv_vnode.erl with some small changes

#Partioned Counter Based on Riak-Core

A counter is a collection of vnode counts. [VNodeCount] 
Each vnode count is a vnode id, vector clock object pair. {VNodeId, VectorClockObject}
Each vector clock object is a vector clock, counter pair. {VectorClock, Counter}

Each time a node updates the value of its counter it updates its VNodeCount object by changing the value of Count and increasing the Vector Clock value by 1. There is only ever one entity updating a VNodeCount object so when merging conflicting VNodeCount objects the one with the highest Vector Clock Value is taken.

For example:

    [{N1, {1, 5}}, {N2, {2, 4}}]               = 9 merged with 
    [{N1, {2, 3}}, {N2, {1, 1}}, {N3, {1, 5}}] = 9 becomes
    ------------------------------------------
    [{N1, {2, 3}}, {N2, {2, 4}}, {N3, {1, 5}}] = 12
    ------------------------------------------

An update is done by the coordinator first updating a single vnode and then propagating that vnodes view of the VNodeCounts to the rest of nodes on the preflist. The rest of the nodes will merge this view with their own view of the VNodeCounts and update their storage.

A read is done by reading from R nodes then merging the results. 

#Issues

1. Whenever a partition changes ownership a new vnode id is created. This can lead to a lot of VNodeCount entries in the counter object because they are never pruned.
2. Repair operations are only done on write and read. If you permanently lose a member of a cluster you should consider forcing a read repair to ensure replicas are maintained. For example if you have N=3, lose two members, then 1 months later lose another member then any keys that haven't been written to or read from in that month may be lost. I suspect this is also the way riak-kv works because i've seen scripts that force read repair on a bucket. But maybe that is for when you want to change the N value on a bucket (http://contrib.basho.com/bucket_inspector.html)  
3. You can't delete a counter object
4. I'm not 100% sure the vnode_id.erl generates ids that are unique enough for my purpose. I may be abusing it :)
5. If you try and start a node that is already running bad things happen. This shouldn't happen.
6. You shouldn't be using this counter implementation anywhere where duplicates or lost updates cannot be tolerated :) For example if you receive an error when updating
   a counter it is possible that the update actually succeeded. This happens because we may write the new counter value to disk and then the network disappears so you
   do not receive confirmation of the write. There is nothing in the protocol to allow safe retrying of a counter operation. (see also: https://issues.apache.org/jira/browse/CASSANDRA-2495)
7. Durability is also problematic. We will try to write the update to one node. If this write succeeds we willtry to write to the other nodes to fullfill the requested W-value. If a subsequent write fails the client receives an error. However, it is possible (most likely!) that the update
will become durable in a subsequent read repair/write repair. The client should probably not retry operations that fail after the first write has
succeeded because it is highly likely to create duplicate counter operations.
8. I have not done enough testing of this code :( The fsms lack unit tests and I found when my vnode lacked unit tests it blew up in spots. It wouldn't surprise if the
   fsms blow up.
9. Failing eunit test for leveldb storage drop. Need to fix this :(

#Running

I couldn't get it to compile with R13B04.Seems to work on R14B01.

    make devrel
    make dev-start
    make dev-join
    make dev1-attach

    ecnty:increment("foo", 1).
    ecnty:get("foo").
    ecnty:increment("foo", 3).
    ecnty:get("foo").

    make dev2-attach

    ecnty:increment("foo", 3).
    ecnty:get("foo").
    ecnty:increment("foo", 5).
    ecnty:get("foo").

