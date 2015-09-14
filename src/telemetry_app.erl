-module(telemetry_app).

-behaviour(application).

-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).


start() ->
  {ok, _} = application:ensure_all_started(telemetry).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  telemetry_sup:start_link().

stop(_State) ->
    ok.