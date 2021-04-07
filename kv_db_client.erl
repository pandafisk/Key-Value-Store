-module(kv_db_client).

-export([create/2, update/2, get/1, delete/1, countKeys/0]).

% start() ->
%     kv_db_server:start().
% stop() ->
%     kv_db_server:stop().
create(Key, Value) ->
    kv_db_supervisor:startChild(Key),
    db_server:put(Key, Value, Key),
    kv_db_supervisor:stopChild(Key).

update(Key, Value) ->
    kv_db_supervisor:startChild(Key),
    db_server:delete(Key, Key),
    db_server:put(Key, Value, Key),
    kv_db_supervisor:stopChild(Key).

get(Key) ->
    kv_db_supervisor:startChild(Key),
    Value = db_server:get(Key, Key),
    kv_db_supervisor:stopChild(Key),
    Value.

delete(Key) ->
    kv_db_supervisor:startChild(Key),
    db_server:delete(Key, Key),
    kv_db_supervisor:stopChild(Key).

countKeys() ->
    kv_db_supervisor:startChild(?MODULE),
    Size = db_server:size(?MODULE),
    kv_db_supervisor:stopChild(?MODULE),
    Size.



%% erl -sname 'server' -setcookies 1234
%% kv_db_supervisor:start_link_from_shell().

%%=========== In another shell ==============
%% erl -sname 'client' -setcookies 1234
%% rpc:call('name', kv_db_client, 'method', [Args]).
 