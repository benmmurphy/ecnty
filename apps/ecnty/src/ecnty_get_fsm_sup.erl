-module(ecnty_get_fsm_sup).
-behaviour(supervisor).

-export([start_fsm/2,start_link/0,init/1]).

start_fsm(Node, Args) ->
  supervisor:start_child({?MODULE, Node}, Args).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Spec = {undefined,
          {ecnty_get_fsm, start_link, []},
           temporary, 5000, worker, [ecnty_get_fsm]},
  {ok, {{simple_one_for_one, 10, 10}, [Spec]}}.

