-module(simple_vclock_test).
-include_lib("eunit/include/eunit.hrl").

create_update_test() ->
  ?assertEqual(1, simple_vclock:update(simple_vclock:new())).

merge_test() ->
  ?assertEqual({4, "hello"}, simple_vclock:merge([
    {2, "bar"},
    {3, "car"},
    {4, "hello"},
    {1, "lols"}])).

scenario_test() ->
  Obj1 = simple_vclock:new_obj("foo"),
  Obj2 = simple_vclock:update_obj(Obj1, "blah"),
  Obj3 = simple_vclock:update_obj(Obj2, "lols"),
  ?assertEqual("lols", simple_vclock:obj(simple_vclock:merge([Obj2, Obj3, Obj1]))).

   
