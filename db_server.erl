-module(db_server).

-behaviour(gen_server).

-export([start_link/0, start/1]).
%%Gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%Public
-export([put/2, get/1, delete/1]).


-record(state, {}).

%% ============================================
%%          Client Call
%% ============================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start(Name) ->
  gen_server:start({local, Name}, ?MODULE, [], []).

put(Key, Value) ->
    gen_server:call(?MODULE, {put, Key, Value}).

get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

delete(Key) ->
    gen_server:call(?MODULE, {delete, Key}).


%% ============================================
%%          Call Back Functions
%% ============================================

init(_Args) -> 
    process_flag(trap_exit, true),
    io:format("~p (~p) starting...~n", [{local, ?MODULE}, self()]),
    db_logic:init(),
    {ok, #state{}}.

handle_call( {put, Key, Value}, _From, State) ->
    db_logic:put(Key, Value),
    io:format("~p (~p) put ~p in DB ~n", [?MODULE, self(),  {Key, Value}]),
    {reply, ok, State};

handle_call({get, Key}, _From, State) ->
    io:format("~p (~p) GET ~n",[?MODULE, self()]),
    Get = db_logic:get(Key),
    [{_, _, Value}] = Get,
    {reply, Value, State};

handle_call({delete, Key}, _From, State) ->
    db_logic:delete(Key),
    io:format("~p (~p) Delete ~p from DB ~n",[?MODULE, self(), Key]),
    {reply, ok, State};


handle_call( _Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) -> 
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.