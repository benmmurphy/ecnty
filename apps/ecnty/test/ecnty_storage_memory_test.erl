-module(ecnty_storage_memory_test).
-include_lib("eunit/include/eunit.hrl").

leveldb_test_() ->
    
    ecnty_storage_test:storage_tests({ecnty_storage_memory, []}, 
        fun() -> ok end).

