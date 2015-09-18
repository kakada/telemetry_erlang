-module(telemetry_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(QUEUE_BOUND, 10).
-record(state, {sock, connected, queue, queue_size}).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

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

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
  gen_server:cast(?SERVER, connect),
  {ok, #state{connected = false, queue = queue:new(), queue_size = 0}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({report, R}, State = #state{connected = false, queue = Q0, queue_size = S0}) ->
  {Q1, S1} = case S0 == ?QUEUE_BOUND of
               true  ->
                 {queue:in(R, queue:drop(Q0)), S0};
               false ->
                 {queue:in(R, Q0), S0 + 1}
             end,
  {noreply, State#state{queue = Q1, queue_size = S1}};

handle_cast({report,{Command, Args}}, State = #state{connected = true, sock = Socket}) ->
  ok = gen_tcp:send(Socket, serialize_command(Command, Args)),
  ok = gen_tcp:send(Socket, "\n"),
  {noreply, State};

handle_cast(connect, State = #state{connected = false, queue = Queue}) ->
  Host = "localhost",
  Port = 1234,

  case gen_tcp:connect(Host, Port, [binary, {delay_send, false}, {nodelay, true}, {buffer, 5}, {active, true}]) of
    {ok, Sock} ->
      clear_queue(Queue),
      {noreply, State#state{sock = Sock, connected = true, queue = queue:new(), queue_size = 0}};
    _ ->
      {ok, _} = timer:apply_after(1000, gen_server, cast, [?SERVER, connect]),
      {noreply, State}
  end.

handle_info({tcp_closed, _}, State) ->
  gen_server:cast(?SERVER, connect),
  {noreply, State#state{connected = false}};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #state{connected = false}) ->
  ok;

terminate(_Reason, #state{connected = true, sock = Socket}) ->
  gen_tcp:close(Socket),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

serialize_command(Command, Args) ->
  Json = {[
    {command, Command},
    {arguments, Args}
  ]},

  jiffy:encode(Json).

clear_queue(Q) ->
  case queue:is_empty(Q) of
    true  -> ok;
    false ->
      {{value, R}, Q1} = queue:out(Q),
      gen_server:cast(?SERVER, {report, R}),
      clear_queue(Q1)
  end.