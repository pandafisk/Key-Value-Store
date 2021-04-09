# Key-Value-Store

This is a distributed and fault-tolerant key-value store for doing basic CRUD operation using mnesia DBMS and gen_supervisor having multiple `gen_server` workers as per demand.


## Instructions

To run the code, compile the files, `db_logic`, `db_server`, `kv_db_client` and `kv_db_supervisor` respectively.

To start the Key-value store on a node, start erlang with a name and set the cookies. For example:  
  
**Step-1**:  
`erl -sname alice -setcookies 1234`  
**Step-2**:  
Run `kv_db_supervisor:start_link_from_shell().`  

**From any running servers**, you can also ready to interact with the DB locally on the node, using the `kv_db_client`-file:

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

## Examples

(After compiling the files as described)

### Starting two server nodes & connecting them

`Shell 1`

```erlang
>erl -sname alice -setcookies 1234

(alice@DESKTOP-BCH0NID)1> kv_db_supervisor:start_link_from_shell().

{local,kv_db_supervisor} (<0.86.0>) starting... 
{local,db_server} (<0.87.0>) starting...        
true
```

`Shell 2`

```erlang
>erl -sname bob -setcookies 1234
(bob@DESKTOP-BCH0NID)1> mnesia:start().
ok
```

`Shell 1`

```erlang
(alice@DESKTOP-BCH0NID)2> db_logic:addReplica('bob@DESKTOP-BCH0NID').

{local,kv_db_supervisor} (<11631.107.0>) starting... 
{local,db_server} (<11631.108.0>) starting...
{atomic,ok}
```

The two nodes `alice@DESKTOP-BCH0NID` and `bob@DESKTOP-BCH0NID` are now connected, and ready to be used as two servers.

### Removing a node from the cluster

To remove a the second node (`bob@DESKTOP-BCH0NID`), in the shell of `alice@DESKTOP-BCH0NID`:

```erlang
(alice@DESKTOP-BCH0NID)3> db_logic:removeReplica('bob@DESKTOP-BCH0NID').

{atomic,ok}
```

### Restarting a server node

If the erlang shell has not been terminated, the server can be restarted with

```erlang
(bob@DESKTOP-BCH0NID)1> mnesia:start().

ok
```

*If the erlang shell has been terminated*, and the local files has not been removed manually, it can be restarted with:

```erlang
> erl -sname bob -setcookies 1234

(bob@DESKTOP-BCH0NID)1> db_logic:restartReplica().

{local,kv_db_supervisor} (<0.135.0>) starting... 
{local,db_server} (<0.136.0>) starting...
true
```

### Remote CRUD

For doing CRUD operation in the database, a client must be connected to a server that has a running DB node.

To connect a client to one of the servers, it should have same cookies and call the `kv_db_client:connect_client/1` with a known server sname. In this way, the client node is not necessarily connecting to that server but it gets a server assigned randomly for further use. 

In a nut shell, `kv_db_client:connect_client/1` method is used to get a server name randomly so that one server doesn't have to take too much load.

```erlang
> erl -sname hans -setcookies 1234

(hans@DESKTOP-BCH0NID)1> Server = kv_db_client:connect_client('alice@DESKTOP-BCH0NID').
'bob@DESKTOP-BCH0NID'
```

(Here we got `bob@DESKTOP-BCH0NID` assigned as our dedicated server)

**Creating an entry:**

`Client Shell`

```erlang
(hans@DESKTOP-BCH0NID)2> kv_db_client:remote_create(Server, age, 22).
ok
```

`Server Shell`

```erlang
(bob@DESKTOP-BCH0NID)2> db_server (<0.136.0>) put {age,22} in DB
```

**Updating an entry:**

`Client Shell`

```erlang
(hans@DESKTOP-BCH0NID)3> kv_db_client:remote_update(Server, age, 23).
ok
```

`Server Shell`

```erlang
(bob@DESKTOP-BCH0NID)2> db_server (<0.136.0>) put {age,23} in DB
```

**Retrieving an entry:**

`Client Shell`

```erlang
(hans@DESKTOP-BCH0NID)4> kv_db_client:remote_get(Server, age).    
23
```

`Server Shell`

```erlang
(bob@DESKTOP-BCH0NID)2> db_server (<0.136.0>) GET 23
```

**Deleting an entry:**

`Client Shell`

```erlang
(hans@DESKTOP-BCH0NID)5> kv_db_client:remote_delete(Server, age). 
ok
```

`Server Shell`

```erlang
(bob@DESKTOP-BCH0NID)2> db_server (<0.136.0>) Delete age from DB
```

**Counting the number of instances in the DB:**

`Client Shell`

```erlang
(hans@DESKTOP-BCH0NID)6> kv_db_client:remote_size(Server).        
0
```

`Server Shell`

```erlang
db_server (<0.136.0>) Size: 0
```
