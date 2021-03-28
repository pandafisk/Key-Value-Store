-module(supervisor_mod).
-behaviour(supervisor).

-export([start_link_shell/0, start_link/0, add_child/1]).
-export([init/1]).

% start_link() ->
%     Pid=supervisor:start_link({local, ?MODULE} , ?MODULE, []),
%     {ok,Pid}.
start_link_shell() ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    unlink(Pid).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
init(_Args) ->
    {ok, {{simple_one_for_one, 10, 60},
          [{kv_db_server, {kv_db_server, start_link, []},
            permanent, brutal_kill, worker, [kv]}]}}.

% add_child(Name)->                                                                        
%     supervisor:start_child(supervisor_mod,
%                            {example_proc, {example_proc, start_link, []},
%                             permanent, brutal_kill, worker, [example_proc]}).

add_child(Name)->                                                                        
    supervisor:start_child(supervisor_mod, []).