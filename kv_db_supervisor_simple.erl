-module(kv_db_supervisor).
-behaviour(supervisor).

-export([start_link/0, start_link_from_shell/0, addReplica/1, removeReplica/1, replicaDetails/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link_from_shell() ->
  {ok, Pid} = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
  unlink(Pid).
start_link()->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []). %If the code doesn't work: change ?SERVER to ?MODULE.

init([]) ->
     io:format("~p (~p) starting... ~n", [{local, ?MODULE}, self()]),

     RestartStrategy = {simple_one_for_one, 5, 60},
     
     ChildSpec = {
                  kv_db_server_proc,
                  {kv_db_server, start_link, []},
                  permanent,
                  infinity,
                  worker,
                  [kv_db_server]
                },
                
    {ok, {RestartStrategy,[ChildSpec]}}.

addReplica(_Args) ->
  {ok, ChildPid} = supervisor:start_child(kv_db_supervisor,[]),
  % Ref = make_ref(),
  register(_Args, ChildPid),
  io:format("Replica named '~p' is created with process id: ~p ~n", [_Args, ChildPid]),
  {ok, ChildPid}.

removeReplica(_Args) ->
  ChildPid = whereis(_Args),
  supervisor:terminate_child(kv_db_supervisor, ChildPid),
  io:format("Replica named '~p' with id: ~p is TERMINATED ~n", [_Args, ChildPid]),
  ok.

replicaDetails() ->
supervisor:which_children(kv_db_supervisor).