-module(db).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-export([start/0, stop/0, put/2, get/1, delete/1, len/0]).

% public functions

start() ->
  gen_server:start_link({global, db}, ?MODULE, [], []).

%% @doc Adds a key-value pair to the database where `Key` is an atom()
%% and `Value` is a term().
put(Key, Value) ->
  gen_server:call(?MODULE, {put, Key, Value}).

%% @doc Fetches `Value` for a given `Key` where `Value` 
%% is a term() and `Key` is an atom().
get(Key) ->
  gen_server:call(?MODULE, {get, Key}).

%% @doc Deletes a key-value pair from the database.
%% `Key` is an atom().
%% Returns the new state of the database or a tuple {error, string()}.
delete(Key) ->
  gen_server:call(?MODULE, {delete, Key}).

%% @doc Returns the length of the database.
len() ->
  io:format( "INFO:~n  Server has ~w key(s) ~n", [length(gen_server:call(?MODULE, len))]).


stop() ->
  gen_server:stop(?MODULE).

% gen_server callbacks

init(_Args) ->
  {ok, kv_db:new()}.

handle_call({put, Key, Value}, _From, State) ->
  NewState = kv_db:put(Key, Value, State),
  {reply, NewState, NewState};

handle_call({get, Key}, _From, State) ->
  {reply, kv_db:get(Key, State), State};

handle_call({delete, Key}, _From, State) ->
  NewState = kv_db:delete(Key, State),
  {reply, NewState, NewState};

handle_call(len, _From, State) ->
  {reply, State, State}.


handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.