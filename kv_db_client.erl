-module(kv_db_client).

-export([create/2, update/2, get/1, delete/1, countKeys/1]).

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
    db_server:put(Key, Value),
    kv_db_supervisor:stopChild(Key).

get(Key) ->
    kv_db_supervisor:startChild(Key),
    db_server:get(Key),
    kv_db_supervisor:stopChild(Key).

delete(Key) ->
    kv_db_supervisor:startChild(Key),
    db_server:delete(Key),
    kv_db_supervisor:stopChild(Key).

countKeys(Key) ->
    kv_db_supervisor:startChild(Key),
    db_server:size(),
    kv_db_supervisor:stopChild(Key).


