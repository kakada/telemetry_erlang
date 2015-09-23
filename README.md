# Telemetry Erlang

[![Build Status](https://travis-ci.org/instedd/telemetry_erlang.svg)](https://travis-ci.org/instedd/telemetry_erlang)

This library allows to record push stats to an [InSTEDD Telemetry Ruby agent](https://github.com/instedd/telemetry_rails) from a separate Erlang component.

## Installation

Add the following dependency in `rebar.config`:

```erlang
{telemetry, ".*", {git, "https://github.com/instedd/telemetry_erlang.git"}}
```

## Usage

Send commands to the agent using `telemetry:report/2`, which receives the command name and a list of arguments in [EEP0018 format](https://github.com/davisp/jiffy#data-format). For example:

```erlang
telemetry:report(counter_add, [
  <<"calls_per_project">>,
  {[{<<"project_id">>, 17}]}
])
```

The client serializes this call and sends the following command over the TCP socket:

```json
{"command": "counter_add", "arguments": ["calls_per_project", {"project_id": 17}]}
```

For more information about the available commands refer to the agent's [Remote API documentation](https://github.com/instedd/telemetry_rails/blob/master/README.md#remote-api).

Calls to `telemetry:report/2` are asynchronous: failures will not interrupt or delay execution of the host application's code. The client monitors the connection to the agent and buffers messages recorded while the connection is down (to send them as soon as it is restored).

## Configuration parameters

The client admits the following configuration parameters:

| Parameter     | Default value |
| ------------- | ------------- |
| agent_host    | 127.0.0.1     |
| agent_port    | 8089          |
| buffer_size   | 10            |

These can be set in `yourapp.config`:

```erlang
{telemetry, [
  {agent_host, "192.168.0.164"},
  {agent_port, 9678},
  {buffer_size, 20}
]},
```

## License

Telemetry Erlang is released under the [GPLv3 license](LICENSE).
