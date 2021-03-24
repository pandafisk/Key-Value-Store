% Defines the module.
-module(kv_db_server).

% Makes the module implement the gen_server behaviour.
-behaviour(gen_server).

% Exports the required gen_server callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% Exports public functions that will ease interaction
% with the gen server.
-export([start/0, put/2, get/1, delete/1, ls/0, size/0, stop/0]).


% public functions

start() ->
  gen_server:start({local, kv_db_server}, ?MODULE, [], []).

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

%% @doc Returns the current state of the database.
ls() ->
  gen_server:call(?MODULE, ls).

size() ->
  io:format( "INFO:~n  DB Server has ~w key(s) ~n", [length(gen_server:call(?MODULE, ls))]).

stop() ->
  gen_server:stop(?MODULE).

% gen_server callbacks

% Module:init(Args) -> Result
%
% This function is called when a gen server process is started.
%
% Args is the list of arguments passed from gen_server:start/3.
% Since we're not using it anywhere in the function implementation
% we're just ignoring it prefixing an underscore to the variable.
%
% It returns {ok, State} where is the internal state of the gen_server.
init(_Args) ->
  {ok, kv_db:new()}.

% Module:handle_call(Request, From, State) -> Result
%
% These functions handle synchronous calls to the gen_server.
%
% Request is an arbitrary term we treat as the message the process receives.
% We are pattern matching on it so we can handle different messages.
%
% From is a tuple {Pid,Tag} where Pid is the pid of the client and Tag is a unique tag.
%
% State is the internal state of the gen_server.
%
% For this particular message {put, Key, Value}, a new key-value pair
% is added to the database adn then we're returning a tuple
% {reply, Reply, NewState} where Reply is what will be given back to From
% and NewState is the gen server's new state.
handle_call({put, Key, Value}, _From, State) ->
  NewState = kv_db:put(Key, Value, State),
  {reply, NewState, NewState};
handle_call({get, Key}, _From, State) ->
  {reply, kv_db:get(Key, State), State};
handle_call({delete, Key}, _From, State) ->
  NewState = kv_db:delete(Key, State),
  {reply, NewState, NewState};
handle_call(ls, _From, State) ->
  {reply, State, State}.

% Module:handle_cast(Request, State) -> Result
%
% This function handles asynchronous calls to the gen_server.
%
% We won't handle any asynchronous calls in this example
% so we're just returning a generic tuple.
handle_cast(_Request, State) ->
  {noreply, State}.

% Module:handle_info(Info, State) -> Result
%
% We won't handle any other incoming message in this example
% so we're just returning a generic tuple.
handle_info(_Info, State) ->
  {noreply, State}.

% Module:terminate(Reason, State)
%
% This function is called when a gen server process is about to terminate.
% It is the opposite of Module:init/1. It's the perfect place to do some clean up handling.
% When it returns, the gen_server terminates with Reason.
%
% The return value is ignored so in this case it just returns the ok atom.
terminate(_Reason, _State) ->
  ok.

% Module:code_change(OldVsn, State, Extra) -> {ok, NewState} | {error, Reason}
%
% This function is called when a gen server process should update its 
% internal state during a release upgrade/downgrade.
%
% In this example we won't do any upgrade/downgrade handling so we're
% just returning a generic tuple.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
