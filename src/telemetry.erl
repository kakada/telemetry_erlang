-module(telemetry).

%% telemetry: library's API.

-export([report/2]).
-export([
  agent_host/0,
  agent_port/0,
  buffer_size/0
]).

%% API

report(Command, Args) ->
  gen_server:cast(agent_connection, {send, {Command, Args}}).

agent_host() ->
  application:get_env(telemetry, agent_host, "127.0.0.1").

agent_port() ->
  application:get_env(telemetry, agent_port, 8089).

buffer_size() ->
  application:get_env(telemetry, buffer_size, 10).