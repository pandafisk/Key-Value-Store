-module(kv_db_client).

%% Internal
-export([create/2, update/2, get/1, delete/1, countKeys/0]).
%% Remote
-export([remote_create/3, remote_update/3, remote_get/2, remote_delete/2, remote_size/1]).
%% Helpers
-export([get_server/0, connect_client/1]).


%% ======================================================
%%               Internal Calls
%% ======================================================
%% These function can only be called internally from a server-node

%% Creates a new entry in the database
create(Key, Value) ->
    db_server:put(Key, Value).

%% Updates an existing entry in the database. Calls the same logic-functin as `create`, but separated for 
%% ease of use for the client  
update(Key, Value) ->
    db_server:delete(Key),
    db_server:put(Key, Value).

%% Retrieves an entry from the database
get(Key) ->
    db_server:get(Key).

%% Deletes an entry in the database
delete(Key) ->
    db_server:delete(Key).

%% Counts the number of keys in the system
countKeys() ->
    db_server:size().

%% ======================================================
%%              Remote Calls
%% ======================================================
%% These functions are intended to be called from a remote client-node
%% Can also be called from a server-node

%% Creates a new entry in the database from a remote client-node
remote_create(Server, Key, Value) ->
    rpc:call(Server, kv_db_client, create, [Key, Value]).

%% Updates an existing entry in the database from a remote client-node.
%% Calls the same logic-function as `remote_create`, but separated for ease of use for the client
remote_update(Server, Key, Value) ->
    rpc:call(Server, kv_db_client, update, [Key, Value]).

%% Retrieves an entry in the database from a remote client-node
remote_get(Server, Key) ->
    rpc:call(Server, kv_db_client, get, [Key]).

%% Deletes an entry in the database from a remote client-node
remote_delete(Server, Key) ->
    rpc:call(Server, kv_db_client, delete, [Key]).

%% Counts the number keys in the system from a remote client-node
remote_size(Server) ->
    rpc:call(Server, kv_db_client, countKeys, []).

%% ======================================================
%%              Helpers
%% ======================================================
%% Helper functions to connect a client to the server

%% Returns a running server to the client
connect_client(Host) ->
    rpc:call(Host, kv_db_client, get_server, []).

%% Selects a random node from one of the servers, to be used as dedicated server for a client
%% Works as a load-balancer
get_server() ->
    Servers = mnesia:table_info(kv_db, where_to_write),
    lists:nth(rand:uniform(length(Servers)), Servers).
