-module(db_logic).

-export([init/0, put/2, get/1, delete/1]).

-record(kv_db,{key, value}).

init() ->
    mnesia:create_schema([]),
    mnesia:start(),
    try
        % mnesia:start()
        mnesia:table_info(type, kv_db)
      
    catch
        exit: _ ->
            mnesia:create_table(kv_db, [{attributes, record_info(fields, kv_db)},
            {type, bag},
            {disc_copies, [node()]}])
    end.

put(Key, Value) ->
    Insert = fun() -> 
        mnesia:write(#kv_db{
            key = Key,
            value = Value})
    end,
   {atomic, Results} =  mnesia:transaction(Insert),
   Results.
    % io:format("Results: ~p~n", [Results]).

get(Key) ->
    Query = fun() -> 
        mnesia:match_object({kv_db, Key, '_'})
    end,
    {atomic, Results} = mnesia:transaction(Query),
    Results.
    % io:format("Results: ~p~n", [Results]).

delete(Key) ->
    Query = 
        fun() -> mnesia:delete({kv_db, Key}) 
        end,
    {atomic, Results} = mnesia:transaction(Query),
    Results.