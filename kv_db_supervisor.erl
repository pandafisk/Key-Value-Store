-module(kv_db_supervisor).
-behaviour(supervisor).

-compile([export_all]).

start_link()->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
     RestartStrategy = {simple_one_for_one, 10, 60},
     ChildSpec = {
                  kv_db_server,
                  {kv_db_server, start_link, []},
                  permanent,
                  brutal_kill,
                  worker,
                  [kv_db_server]
                },
    {ok, {RestartStrategy,[ChildSpec]}}.