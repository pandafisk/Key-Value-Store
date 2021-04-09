-module(kv_db_supervisor).
-behaviour(supervisor).

-export([start_link/0, start_link_from_shell/0, startChild/1, stopChild/1, restartChild/1, listChildren/0, deleteChildSpec/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

%% Public function to start the server
start_link_from_shell() ->
  {ok, Pid} = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
  unlink(Pid).
start_link()->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []). 

%% The starting function, setting the parameters for the node to become a server-node,
%% herein setting the restart-strategy for the supervisor, and the parmeters of the child-node
init([]) ->
     io:format("~p (~p) starting... ~n", [{local, ?MODULE}, self()]),

     RestartStrategy = {one_for_one, 3, 3},
     
     ChildSpec = {
                  db_server_proc,
                  {db_server, start_link, []},
                  permanent,
                  infinity,
                  supervisor,
                  [db_server]
                },
                
    {ok, {RestartStrategy,[ChildSpec]}}.


%% Start, stops, restarts, and deletes children, as well as lists the children of the supervisor.
%% USED FOR DEBUGGING PURPOSES  
startChild(ID) ->
  ChildSpec = {
                  ID,
                  {db_server, start, [ID]},
                  temporary, 
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
