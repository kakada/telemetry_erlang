-module(telemetry_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  ServerSpecs = [
    {
      telemetry_srv,
      {telemetry_srv, start_link, []},
      permanent,
      5000,
      worker,
      [telemetry_srv]
    },
    {
      buffer_srv,
      {buffer_srv, start_link, []},
      permanent,
      5000,
      worker,
      [buffer_srv]
    }
  ],
  {ok, { {one_for_one, 5, 10}, ServerSpecs} }.
