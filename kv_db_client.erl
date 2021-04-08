-module(kv_db_client).

%% Internal
-export([create/2, update/2, get/1, delete/1, countKeys/0]).
%% Remote
-export([remote_create/3, remote_update/3, remote_get/2, remote_delete/2, remote_size/1]).
%% Helpers
-export([get_server/0, connect_client/1]).


%% ======================================================
%%               Internal Calls
%% ======================================================

% start() ->
%     kv_db_server:start().
% stop() ->
%     kv_db_server:stop().
create(Key, Value) ->
    % kv_db_supervisor:startChild(Key),
    db_server:put(Key, Value).
    % kv_db_supervisor:stopChild(Key).

update(Key, Value) ->
    % kv_db_supervisor:startChild(Key),
    db_server:delete(Key),
    db_server:put(Key, Value).
    % kv_db_supervisor:stopChild(Key).

get(Key) ->
    % kv_db_supervisor:startChild(Key),
    db_server:get(Key).
    % kv_db_supervisor:stopChild(Key),
    % Value.

delete(Key) ->
    % kv_db_supervisor:startChild(Key),
    db_server:delete(Key).
    % kv_db_supervisor:stopChild(Key).

countKeys() ->
    % kv_db_supervisor:startChild(?MODULE),
    db_server:size().
    % kv_db_supervisor:stopChild(?MODULE),
    % Size.

%% ======================================================
%%              Remote Calls
%% ======================================================

remote_create(Server, Key, Value) ->
    rpc:call(Server, kv_db_client, create, [Key, Value]).

remote_update(Server, Key, Value) ->
    rpc:call(Server, kv_db_client, update, [Key, Value]).

remote_get(Server, Key) ->
    rpc:call(Server, kv_db_client, get, [Key]).

remote_delete(Server, Key) ->
    rpc:call(Server, kv_db_client, delete, [Key]).

remote_size(Server) ->
    rpc:call(Server, kv_db_client, countKeys, []).

%% ======================================================
%%              Helpers
%% ======================================================

connect_client(Host) ->
    rpc:call(Host, kv_db_client, get_server, []).

get_server() ->
    Servers = mnesia:table_info(kv_db, where_to_write),
    lists:nth(rand:uniform(length(Servers)), Servers).




%% erl -sname 'server' -setcookies 1234
%% kv_db_supervisor:start_link_from_shell().

%%=========== In another shell ==============
%% erl -sname 'client' -setcookies 1234
%% rpc:call('name', kv_db_client, 'method', [Args]).