-module(ecnty_storage_leveldb).
-behaviour(ecnty_storage).
-export([start/2, stop/1, put/3, get/2, drop/1, fold_objects/3, is_empty/1]).

-record(state, {leveldb_handle, file}).

parse_value(Binary) ->
    binary_to_term(Binary).

create_value(Term) ->
    term_to_binary(Term).

create_key(Term) ->
    term_to_binary(Term).

parse_key(Binary) ->
    binary_to_term(Binary).

start(Partition, Config) ->
    case config_value(data_root, Config) of
        undefined ->
            lager:error("Failed to get data_root"),
            {error, data_root};
        DataRoot ->
            File = filename:join(DataRoot, integer_to_list(Partition)),
            open(File, #state{file = File})
    end.
    
config_value(Key, Config) ->
    config_value(Key, Config, undefined).

config_value(Key, Config, Default) ->
    case proplists:get_value(Key, Config) of
        undefined ->
            app_helper:get_env(leveldb, Key, Default);
        Value ->
            Value
    end.
        
open(File, State) ->
    ok = filelib:ensure_dir(filename:join(File, "X")),
    case eleveldb:open(File, [{create_if_missing, true}]) of
      {ok, LevelDBHandle} ->
          {ok, State#state{leveldb_handle = LevelDBHandle}};
      {error, Reason} ->
          {error, Reason}
     end. 

stop(_State) ->
    %% this looks very weird. when gc collects all the leveldb objects that hold onto the db ref
    %% the db ref will be deleted in c++ land and all the file locks will be released. 
    %% this makes me feel really dirty..... maybe this is standard erlang way to do things
    %% but coming from other gc managed languages this smells funny.
    ok. 

get(#state{leveldb_handle=Handle}, Key) ->
    case eleveldb:get(Handle, create_key(Key), []) of
        not_found -> {error, not_found};
        {error, Reason} -> {error, Reason};
        {ok, Binary} -> {ok, parse_value(Binary)}
    end.

put(#state{leveldb_handle=Handle}=State, Key, Value) ->
    {eleveldb:put(Handle, create_key(Key), create_value(Value), []), State}.


is_empty(#state{leveldb_handle=Handle}) ->
    eleveldb:is_empty(Handle).

drop(#state{file = File} = State) ->
    case eleveldb:destroy(File, []) of
      ok -> 
          case open(File, State) of
              {error, Reason} ->
                  {error, Reason, State};
              {ok, State} ->
                  {ok, State}
          end;
      {error, Reason} ->
          {error, Reason, State}
    end.

fold_fun(FoldFun) ->
    fun ({Key, Value}, Acc) ->
        FoldFun(parse_key(Key), parse_value(Value), Acc)
    end.

fold_objects(#state{leveldb_handle = Handle}, FoldFun, Acc) ->
    {ok, eleveldb:fold(Handle, fold_fun(FoldFun), Acc, [])}.


