-module(db_logic).

-export([initDB/0, put/2, get/1, delete/1]).

-record(kv_db,{key, value}).

initDB() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    try
        mnesia:table_info(type, kv_db)
    catch
        exit: _ ->
            mnesia:create_table(kv_db, [{attributes, record_info(fields, kv_db)}])
            % {type, bag},
            % {disc_copies, [node()]}
    end.

put(Key, Value) ->
    Insert = fun() -> 
        mnesia:write(#kv_db{
            key = Key,
            value = Value})
    end,
    mnesia:transaction(Insert).
    % io:format("Results: ~p~n", [Results]).

get(Key) ->
    Query = fun() -> 
        mnesia:match_object({kv_db, Key, '_'})
    end,
    mnesia:transaction(Query).
    % io:format("Results: ~p~n", [Results]).

delete(Key) ->
    Query = 
        fun() -> mnesia:delete({kv_db, Key}) end,
    {atomic, _} = mnesia:transaction(Query). 