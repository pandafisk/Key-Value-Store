-module(kv_db_client).

-export([create/2, update/2, getValueByKey/1, deleteValueByKey/1, totalNumberOfKey/0]).

% start() ->
%     kv_db_server:start().
% stop() ->
%     kv_db_server:stop().
create(Key, Value) ->
    io:format("Client Call: ~n~p (~p) starting... ~n", [{local, ?MODULE}, self()]),
    db_server:put(Key, Value).

update(Key, Value) ->
    db_server:put(Key, Value).

getValueByKey(Key) ->
    db_server:get(Key).

deleteValueByKey(Key) ->
    db_server:delete(Key).

totalNumberOfKey() ->
    db_server:size().
