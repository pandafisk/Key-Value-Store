# Key-Value-Store

To run the code, compile the files, `db_logic`, `db_server`, `kv_db_client` & `kv_db_supervisor`


#### Instructions
To start the Key-value store, run `kv_db_supervisor:start_link_from_client()`.

To interract with the DB, use the `kv_db_client`-file:
- `create/2`: Creates a new entry in the DataBase.
- `update/2`: Updates an entry in the DataBase.
- `get/1`: Fetches the value from the given key.
- `delete/1`: Deletes an entry with the given key.