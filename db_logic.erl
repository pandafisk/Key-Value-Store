-module(db_logic).

-export([init/0, put/2, get/1, delete/1, size/0, addReplica/1, removeReplica/1, restartReplica/0]).

-record(kv_db,{key, value}).

%% Initialises the DB 
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

%%====================================================================
%% mnesia logic function for CRUD operations.
%% Creates an anonymous function to make an mnesia transaction for the 
%% given operation, PUT, GET or DELETE

put(Key, Value) ->
    Insert = fun() -> 
        mnesia:write(#kv_db{
            key = Key,
            value = Value})
    end,
   {atomic, Results} =  mnesia:transaction(Insert),
   Results.

get(Key) ->
    Query = fun() -> 
        mnesia:match_object({kv_db, Key, '_'})
    end,
    {atomic, Results} = mnesia:transaction(Query),
    Results.

delete(Key) ->
    Query = fun() -> 
        mnesia:delete({kv_db, Key}) 
    end,
    {atomic, Results} = mnesia:transaction(Query),
    Results.
%%====================================================================

%% mnesia logic function for returning the size of the DB
size() ->
    mnesia:table_info(kv_db, size).

%% Adding a node to the cluster of server-nodes. 
%% Remotely start mnesia, and a server-supervisor, before copying the DB to disk, so all 
%% server-nodes can read from it
addReplica(NodeName) ->
    rpc:call(NodeName, db_logic, restartReplica, []),
    mnesia:change_config(extra_db_nodes, [NodeName]),
    mnesia:change_table_copy_type(schema, NodeName, disc_copies),
    mnesia:add_table_copy(kv_db, NodeName, disc_copies).

%% Removes a given node from the cluster of active server-nodes
removeReplica(Nodename) ->
    rpc:call(Nodename, mnesia, stop, []),
    mnesia:del_table_copy(schema, Nodename).

%% Restarts a node as a server, in case of a shut-down
restartReplica() ->
    mnesia:start(),
    kv_db_supervisor:start_link_from_shell().