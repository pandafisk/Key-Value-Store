# Key-Value-Store

To run the code, compile the files, `db_logic`, `db_server`, `kv_db_client` & `kv_db_supervisor`

## Instructions

To start the Key-value store on a node, start erlang with a name and set the cookies. for example:

`erl -sname alice -setcookies 1234`

**From any running DB nodes**, run `kv_db_supervisor:start_link_from_shell().`, and you are ready to interact with the DB locally on the node, using the `kv_db_client`-file:

- `create/2`: Creates a new entry in the DataBase.
- `update/2`: Updates an entry in the DataBase.
- `get/1`: Fetches the value from the given key.
- `delete/1`: Deletes an entry with the given key.
- `countKeys/0`: Returns the size of the DB.

**Connecting another node as another server:**

1. Start erlang with a name, and the same cookies as the other server-nodes. for example: `erl -sname bob -setcookies 1234`.
2. Call `mnesia:start().` from the new node (`bob`).
3. Call `db_logic:addReplica(nodename)` from any running server-nodes (excluding `bob` as its not a server node yet).
4. Run `kv_db_supervisor:start_link_from_shell().` from the new node (`bob`).
5. The new node (`bob`) is now ready as a server-node.

**Connecting a client to the server to do CRUD operation remotely:**

Use the following functions from `kv_db_client`:

- `connect_client/1`: Returns a running server-node, which can be saved in the client-node and used in the other remote calls. (`Host` must be a running server-node)
- `remote_create/3`: Creates a new entry in the DataBase. (`Server` must be any running server-node, such as one returned from `connect_client/1`)
- `remote_update/3`: Updates an entry in the DataBase. (`Server` must be any running server-node, such as one returned from `connect_client/1`)
- `remote_get/2`: Fetches the value from the given key. (`Server` must be any running server-node, such as one returned from `connect_client/1`)
- `remote_delete/2`: Deletes an entry with the given key. (`Server` must be any running server-node, such as one returned from `connect_client/1`)
- `remote_countKeys/1`: Returns the size of the DB. (`Server` must be any running server-node, such as one returned from `connect_client/1`)
