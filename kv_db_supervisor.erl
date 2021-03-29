-module(kv_db_supervisor).
-behaviour(supervisor).

-export([start_link/0, start_link_from_shell/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link_from_shell() ->
  {ok, Pid} = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
  unlink(Pid).
start_link()->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
     io:format("~p (~p) starting... ~n", [{global, ?MODULE}, self()]),

     RestartStrategy = {simple_one_for_one, 10, 60},
     
     ChildSpec = {
                  kv_db_server_proc,
                  {kv_db_server, start_link, []},
                  permanent,
                  infinity,
                  worker,
                  [kv_db_server]
                },
                
    {ok, {RestartStrategy,[ChildSpec]}}.