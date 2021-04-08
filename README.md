# Key-Value-Store

To run the code, compile the files, `db_logic`, `db_server`, `kv_db_client` & `kv_db_supervisor`

## Instructions

To start the Key-value store on a node, start erlang with a name and set the cookies. for example:

`erl -sname alice -setcookies 1234`

From any running DB nodes, Run `kv_db_supervisor:start_link_from_shell()`, and you are ready to interact with the DB locally on the node, using the `kv_db_client`-file:

- `create/2`: Creates a new entry in the DataBase.
- `update/2`: Updates an entry in the DataBase.
- `get/1`: Fetches the value from the given key.
- `delete/1`: Deletes an entry with the given key.
- `countKeys/0`: Returns the size of the DB.

**Connecting another node as another server:**

1. Start erlang with a name, and the same cookies as the other server-nodes. for example: `erl -sname bob -setcookies 1234`.
2. Call `db_logic:addReplica(nodename)` from any running server-nodes (excluding `bob` as its not a server node yet).
3. The new node (`bob`) is now ready as a server-node.

**Removing a node from the cluster:**

Simply call `db_logic:removeReplica/1` from any other running server-node, and the specified node will be removed from the cluster.

**Connecting a client to the server to do CRUD operation remotely:**

Use the following functions from `kv_db_client`:

- `connect_client/1`: Returns a running server-node, which can be saved in the client-node and used in the other remote calls. (`Host` must be a running server-node)
- `remote_create/3`: Creates a new entry in the DataBase. (`Server` must be any running server-node, such as one returned from `connect_client/1`)
- `remote_update/3`: Updates an entry in the DataBase. (`Server` must be any running server-node, such as one returned from `connect_client/1`)
- `remote_get/2`: Fetches the value from the given key. (`Server` must be any running server-node, such as one returned from `connect_client/1`)
- `remote_delete/2`: Deletes an entry with the given key. (`Server` must be any running server-node, such as one returned from `connect_client/1`)
- `remote_countKeys/1`: Returns the size of the DB. (`Server` must be any running server-node, such as one returned from `connect_client/1`)

## Examples

(After compiling the files as described)

### Starting two server nodes & connecting them

`Shell 1`

```shell
>erl -sname alice -setcookies 1234

(alice@DESKTOP-BCH0NID)1> kv_db_supervisor:start_link_from_shell().

{local,kv_db_supervisor} (<0.86.0>) starting... 
{local,db_server} (<0.87.0>) starting...        
true
```

`Shell 2`

```shell
>erl -sname bob -setcookies 1234
```

`Shell 1`

```shell
(alice@DESKTOP-BCH0NID)2> db_logic:addReplica('bob@DESKTOP-BCH0NID').

{local,kv_db_supervisor} (<11631.107.0>) starting... 
{local,db_server} (<11631.108.0>) starting...
{atomic,ok}
```

The two nodes `alice@DESKTOP-BCH0NID` and `bob@DESKTOP-BCH0NID` are now connected, and ready to be used as two servers.

### Removing a node from the cluster

To remove a the second node (`bob@DESKTOP-BCH0NID`), in the shell of `alice@DESKTOP-BCH0NID`:

```shell
(alice@DESKTOP-BCH0NID)3> db_logic:removeReplica('bob@DESKTOP-BCH0NID').

{atomic,ok}
```

### Restarting a server node

If the erlang shell has not been terminated, the server can be restarted with

```erlang
(bob@DESKTOP-BCH0NID)1> mnesia:start().

ok
```

If the erlang shell has been terminated, and the local files has not been removed manually, it can be restarted with:

```shell
> erl -sname bob -setcookies 1234

(bob@DESKTOP-BCH0NID)1> db_logic:restartReplica().

{local,kv_db_supervisor} (<0.135.0>) starting... 
{local,db_server} (<0.136.0>) starting...
true
```

### Remote CRUD

To connect a client to one of the servers start the erlang shell with a name, and same cookies as previously, and call the `kv_db_client:connect_client/1` to get a server assigned for further use:

```erlang
> erl -sname hans -setcookies 1234

(hans@DESKTOP-BCH0NID)1> Server = kv_db_client:connect_client('alice@DESKTOP-BCH0NID').
'bob@DESKTOP-BCH0NID'
```

(Here we got `bob@DESKTOP-BCH0NID` assigned as our dedicated server)

**Creating an entry:**

`Client Shell`

```shell
(hans@DESKTOP-BCH0NID)2> kv_db_client:remote_create(Server, age, 22).
ok
```

`Server Shell`

```shell
(bob@DESKTOP-BCH0NID)2> db_server (<0.136.0>) put {age,22} in DB
```

**Updating an entry:**

`Client Shell`

```shell
(hans@DESKTOP-BCH0NID)3> kv_db_client:remote_update(Server, age, 23).
ok
```

`Server Shell`

```shell
(bob@DESKTOP-BCH0NID)2> db_server (<0.136.0>) put {age,23} in DB
```

**Retrieving an entry:**

`Client Shell`

```shell
(hans@DESKTOP-BCH0NID)4> kv_db_client:remote_get(Server, age).    
23
```

`Server Shell`

```shell
(bob@DESKTOP-BCH0NID)2> db_server (<0.136.0>) GET 23
```

**Deleting an entry:**

`Client Shell`

```shell
(hans@DESKTOP-BCH0NID)5> kv_db_client:remote_delete(Server, age). 
ok
```

`Server Shell`

```shell
(bob@DESKTOP-BCH0NID)2> db_server (<0.136.0>) Delete age from DB
```

**Counting the number of instances in the DB:**

`client Shell`

```shell
(hans@DESKTOP-BCH0NID)6> kv_db_client:remote_size(Server).        
0
```

`Server Shell`

```shell
db_server (<0.136.0>) Size: 0
```
