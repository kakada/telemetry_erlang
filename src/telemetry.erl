-module(telemetry).

%% telemetry: telemetry library's entry point.

-export([report/2]).

%% API

report(Command, Args) ->
  gen_server:cast(telemetry_srv, {report, {Command, Args}}).
