-module(agent_connection).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {sock, connected}).
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
  {ok, #state{connected = false}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({send, R}, State = #state{connected = false}) ->
  buffer:add(R),
  {noreply, State};

handle_cast({send,{Command, Args}}, State = #state{connected = true, sock = Socket}) ->
  ok = gen_tcp:send(Socket, serialize_command(Command, Args)),
  ok = gen_tcp:send(Socket, "\n"),
  {noreply, State};

handle_cast(connect, State = #state{connected = false}) ->
  SocketConfig = [binary, {delay_send, false}, {nodelay, true}, {buffer, 5}, {active, true}],
  case gen_tcp:connect(telemetry:agent_host(), telemetry:agent_port(), SocketConfig) of
    {ok, Sock} ->
      buffer:retry_all(),
      {noreply, State#state{sock = Sock, connected = true}};
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