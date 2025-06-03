# gun\_pool

`gun_pool` is an Erlang library that provides an abstraction layer for managing HTTP/1.1 and HTTP/2 connection pools using the `gun` client. It enables efficient connection reuse and simplifies concurrent HTTP request handling.

## Features

* Dynamic management of named connection pools.
* Support for HTTP/1.1 and HTTP/2 (including TLS).
* Flexible configuration of pool sizes and connection options.
* Automatic retries and connection reuse.
* Simple API for sending requests with or without a body.
* Automatic JSON response detection and decoding.
* Optional customization of connection and request options.
* Supervision and automatic recovery using OTP supervisors.

## Usage

### Start a pool

```erlang
ok = gun_pool:start_pool(my_pool).
```

With custom options:

```erlang
Opts = [
    {pool_size, 20},
    {pool_max_overflow, 10},
    {connection_host, "api.example.com"},
    {connection_port, 443},
    {connection_opts, #{connect_timeout => 5000, transport => tls}}
],
ok = gun_pool:start_pool(my_pool, Opts).
```

### Send a request

```erlang
{ok, Status, Headers, Body} =
    gun_pool:send_request(my_pool, get, <<"/api/ping">>, []).
```

With a body:

```erlang
Body = #{<<"key">> => <<"value">>},
{ok, Status, Headers, RespBody} =
    gun_pool:send_request(my_pool, post, <<"/api/data">>, [], Body).
```

With a body and request options (`gun:req_opts()`):

```erlang
ReqOpts = #{flow => 128},
{ok, Status, Headers, RespBody} =
    gun_pool:send_request(my_pool, post, <<"/api/data">>, [], Body, ReqOpts).
```

### Stop a pool

```erlang
ok = gun_pool:stop_pool(my_pool).
```


## Configuration

Available options for pool configuration include:

| Option              | Type        | Description                                                                                |
| ------------------- | ----------- | ------------------------------------------------------------------------------------------ |
| `pool_size` | `pos_integer()` | Number of active connections in the pool |
| `pool_max_overflow` | `non_neg_integer()` | Maximum number of extra connections |
| `connection_host` | `string()` | Host for connections |
| `connection_port` | `inet:port_number()` | Port for connections |
| `connection_opts` | `gun:opts()` | Advanced connection options for gun |

## Tests

To run the tests:

```bash
rebar3 ct
```

## Lint / Code Quality Checks

To run code formatting checks, cross-reference analysis, Dialyzer, and Gradualizer, use the lint alias:

```bash
rebar3 lint
```

This will run the following checks:

* Code formatting verification (rebar3 fmt --check)
* Cross-reference analysis (rebar3 xref)
* Dialyzer static type analysis (rebar3 dialyzer)
* Gradualizer gradual typing checks (rebar3 gradualizer)
