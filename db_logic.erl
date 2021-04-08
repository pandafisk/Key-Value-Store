-module(db_logic).

-export([init/0, put/2, get/1, delete/1, size/0, addReplica/1, removeReplica/1, restartReplica/0]).

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

size() ->
    mnesia:table_info(kv_db, size).


% STEP 1 Start Mnesia in the "new node" which you want to add as replica 
% STEP 2 Call addReplica(Nodename) on any running db nodes except "new node"
% STEP 3 Run "kv_db_supervisor:start_link_from_shell()."  from the "new node"
% STEP 4 Access "new node" or any other replica from client node to do any CRUD operation
addReplica(NodeName) ->
    rpc:call(NodeName, db_logic, restartReplica, []),
    % add a node to Mnesia
    mnesia:change_config(extra_db_nodes, [NodeName]),
    % move the database schema to disc for the new node to make it persistent
    mnesia:change_table_copy_type(schema, NodeName, disc_copies),
    %To make new the node capable of storing disc copies
    mnesia:add_table_copy(kv_db, NodeName, disc_copies).

removeReplica(Nodename) ->
    rpc:call(Nodename, mnesia, stop, []),
    mnesia:del_table_copy(schema, Nodename).


%Call this method from the node itself to restart itself as replica
restartReplica() ->
    mnesia:start(),
    kv_db_supervisor:start_link_from_shell().