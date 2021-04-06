-module(kv_db_supervisor).
-behaviour(supervisor).

-export([start_link/0, start_link_from_shell/0, startChild/1, stopChild/1, restartChild/1, listChildren/0, deleteChildSpec/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link_from_shell() ->
  {ok, Pid} = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
  unlink(Pid).
start_link()->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []). %If the code doesn't work: change ?SERVER to ?MODULE.

init([]) ->
     io:format("~p (~p) starting... ~n", [{local, ?MODULE}, self()]),

     RestartStrategy = {one_for_one, 3, 3},
     
     ChildSpec = {
                  kv_db_server_proc,
                  {db_server, start_link, []},
                  permanent,
                  infinity,
                  supervisor,
                  [db_server]
                },
                
    {ok, {RestartStrategy,[ChildSpec]}}.

startChild(ID) ->
  ChildSpec = {
                  ID,
                  {db_server, start, [ID]},
                  permanent, %We don't want the child_spec to be removed, so we can restrat it.
                  brutal_kill,
                  worker,
                  [db_server]
                },
  supervisor:start_child(?MODULE, ChildSpec).

stopChild(ID) ->
  supervisor:terminate_child(?MODULE, ID).

restartChild(ID) ->
  supervisor:restart_child(?MODULE, ID).

deleteChildSpec(ID) ->
  supervisor:delete_child(?MODULE, ID).

listChildren() ->
  supervisor:which_children(?MODULE).
