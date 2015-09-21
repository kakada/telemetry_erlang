-module(buffer).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {queue, queue_size}).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([add/1, retry_all/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add(Item) ->
  gen_server:call(?SERVER, {add, Item}).

retry_all() ->
  gen_server:call(?SERVER, retry_all).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
  {ok, #state{queue = queue:new(), queue_size = 0}}.

handle_call({add, Item}, _From, #state{queue = Q0, queue_size = S0}) ->
  {Q1, S1} = case S0 == telemetry:buffer_size() of
               true  ->
                 {queue:in(Item, queue:drop(Q0)), S0};
               false ->
                 {queue:in(Item, Q0), S0 + 1}
             end,
  {reply, ok, #state{queue = Q1, queue_size = S1}};

handle_call(retry_all, _From, #state{queue = Q}) ->
  clear_queue(Q),
  {reply, ok, #state{queue = queue:new(), queue_size = 0}}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

clear_queue(Q) ->
  case queue:is_empty(Q) of
    true  -> ok;
    false ->
      {{value, Item}, Q1} = queue:out(Q),
      gen_server:cast(agent_connection, {send, Item}),
      clear_queue(Q1)
  end.