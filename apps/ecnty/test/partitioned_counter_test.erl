-module(partitioned_counter_test).
-include_lib("eunit/include/eunit.hrl").

update_test() ->
  Counters1 = partitioned_counter:update("my_id", 12, []),
  ?debugVal( Counters1),
  Counters2 = partitioned_counter:update("my_id", 10, Counters1),
  ?debugVal(Counters2),
  Counters3 = partitioned_counter:update("foo", 5, Counters2),
  ?debugVal(Counters3),
  Counters4 = partitioned_counter:update("foo", 7, Counters3),
  ?assertEqual(34, partitioned_counter:value(Counters4)),
  ?assertEqual({"foo", {1,12}}, lists:keyfind("foo", 1, Counters4)),
  ?assertEqual({"my_id", {1,22}}, lists:keyfind("my_id", 1, Counters4)).

merge_test() ->
  CountersA = partitioned_counter:new("my_id", 12),
  CountersB = partitioned_counter:new("foo", 5),
  CountersA2 = partitioned_counter:update("my_id", 6, CountersA),
  CountersB2 = partitioned_counter:update("foo", 7, CountersB),
  CountersA2Merged = partitioned_counter:merge([CountersA2, CountersB]),
  ?debugVal(CountersA2Merged),
  CountersB2Merged = partitioned_counter:merge([CountersB2, CountersA]),
  ?debugVal(CountersB2Merged),
  CountersMerged = partitioned_counter:merge([CountersA2Merged, CountersB2Merged]),
  ?debugVal(CountersMerged),
  ?assertEqual(30, partitioned_counter:value(CountersMerged)).
  
