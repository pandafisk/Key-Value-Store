-module(kv_db_client).

%% Local
-export([create/2, update/2, get/1, delete/1, countKeys/0]).

%% Remote
-export([remoteCreate/2, remoteUpdate/2, remoteGet/1, remoteDelete/1, remoteSize/0]).


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

%%==================================================
%%              Remote Client
%%==================================================

remoteCreate(Key, Value) ->
    Servers = mnesia:table_info(kv_db, where_to_write),
    Server = lists:nth(rand:uniform(length(Servers)), Servers),
    rpc:call(Server, kv_db_client, create, [Key, Value]).

remoteUpdate(Key, Value) ->
    Servers = mnesia:table_info(kv_db, where_to_write),
    Server = lists:nth(rand:uniform(length(Servers)), Servers),
    rpc:call(Server, kv_db_client, update, [Key, Value]).

remoteGet(Key) ->
    Servers = mnesia:table_info(kv_db, where_to_write),
    Server = lists:nth(rand:uniform(length(Servers)), Servers),
    rpc:call(Server, kv_db_client, get, [Key]).

remoteDelete(Key) ->
    Servers = mnesia:table_info(kv_db, where_to_write),
    Server = lists:nth(rand:uniform(length(Servers)), Servers),
    rpc:call(Server, kv_db_client, delete, [Key]).

remoteSize() -> 
    Servers = mnesia:table_info(kv_db, where_to_write),
    Server = lists:nth(rand:uniform(length(Servers)), Servers),
    rpc:call(Server, kv_db_client, countKeys, []).


%% erl -sname 'server' -setcookies 1234
%% kv_db_supervisor:start_link_from_shell().

%%=========== In another shell ==============
%% erl -sname 'client' -setcookies 1234
%% rpc:call('name', kv_db_client, 'method', [Args]).
