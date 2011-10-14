-module(ecnty_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
    VMaster = { ecnty_vnode_master,
                  {riak_core_vnode_master, start_link, [ecnty_vnode]},
                  permanent, 5000, worker, [riak_core_vnode_master]},
    IncrementFSM = {ecnty_increment_fsm_sup,
                   {ecnty_increment_fsm_sup, start_link, []},
                    permanent, infinity, supervisor, [ecnty_increment_fsm_sup]},
    GetFSM       = {ecnty_get_fsm_sup,
                   {ecnty_get_fsm_sup, start_link, []},
                    permanent, infinity, supervisor, [ecnty_get_fsm_sup]},

    { ok,
        { {one_for_one, 5, 10},
          [VMaster, IncrementFSM, GetFSM]}}.
