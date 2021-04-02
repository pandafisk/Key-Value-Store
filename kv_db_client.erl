-module(kv_db_client).

-export([create/2, update/2, getValueByKey/1, deleteValueByKey/1, totalNumberOfKey/0]).

% start() ->
%     kv_db_server:start().
% stop() ->
%     kv_db_server:stop().
create(Key, Value) ->
    io:format("~p (~p) starting... ~n", [{local, ?MODULE}, self()]),
    kv_db_server:put(Key, Value).

update(Key, Value) ->
    kv_db_server:put(Key, Value).

getValueByKey(Key) ->
    kv_db_server:get(Key).

deleteValueByKey(Key) ->
    kv_db_server:delete(Key).

totalNumberOfKey() ->
    kv_db_server:size().
