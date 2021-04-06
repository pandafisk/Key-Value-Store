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

countKeys(Key) ->
    kv_db_supervisor:startChild(Key),
    db_server:size(),
    kv_db_supervisor:stopChild(Key).



%% erl -sname 'name' -setcookies 1234
%% kv_db_supervisor:start_link_from_shell().

%%=========== In another shell ==============
%% erl -sname 'name2' -setcookies 1234
%% rpc:call('name', kv_db_client, 'method', [Args]).
 