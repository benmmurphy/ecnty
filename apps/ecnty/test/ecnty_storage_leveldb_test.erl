-module(ecnty_storage_leveldb_test).
-include_lib("eunit/include/eunit.hrl").

leveldb_test_() ->
    ecnty_storage_test:storage_tests({ecnty_storage_leveldb, [{data_root, "test_data_root"}]}, 
        fun() -> ?cmd("rm -rf test_data_root") end).

