%% @author David González Piñeiro
%% @license Apache License, Version 2.0
%% @see https://github.com/DavidGonzalezPineiro/gun_pool
%% See the LICENSE file in the project root for more information.

-module(gun_pool_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile([export_all, nowarn_export_all]).

all() ->
    [
        app_starts_test,
        supervisor_running_test,
        gun_pool_start_without_args_test,
        gun_pool_start_http2_test,
        gun_pool_start_unexisting_server_error_test,
        gun_pool_multiple_start_test,
        gun_pool_send_requests_test,
        gun_pool_send_request_error_test,
        gun_pool_send_json_body_request_test,
        gun_pool_send_body_request_opts_test,
        gun_pool_stop_test
    ].

load_ct_config() ->
    Apps = ct:get_config(apps, []),
    Env = ct:get_config(env, []),
    [ok = application:load(App) || App <- Apps],
    [ok = application:set_env(App, K, V) || {App, KeyVal} <- Env, {K, V} <- KeyVal],
    ok.

init_per_suite(Config) ->
    ok = load_ct_config(),
    application:ensure_all_started(gun_pool),
    application:ensure_all_started(cowboy),
    application:ensure_all_started(ranch),
    {ok, Module} = compile:file("../../lib/gun_pool/test/stubs/mock_http_server"),
    ok = Module:start(8080),
    Config.

end_per_suite(_Config) ->
    application:stop(gun_pool),
    ok.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

app_starts_test(_Config) ->
    Apps = application:which_applications(),
    {gun_pool, _, _} = lists:keyfind(gun_pool, 1, Apps).

supervisor_running_test(_Config) ->
    Pid = whereis(gun_pool_sup),
    true = is_process_alive(Pid).

gun_pool_start_without_args_test(_Config) ->
    PoolName = test_pool,
    {ok, _} = gun_pool:start_pool(PoolName),
    ok = gun_pool:stop_pool(PoolName).

gun_pool_start_http2_test(_Config) ->
    {ok, Module} = compile:file("../../lib/gun_pool/test/stubs/mock_http_server"),
    ok = Module:stop(),
    ok = Module:start_tls(8082),
    PoolName = test_pool,
    Opts = #{
        transport => tls,
        protocols => [http2],
        tls_opts => [
            {verify, verify_none},
            {versions, ['tlsv1.2', 'tlsv1.3']}
        ]
    },
    Options = [{pool_max_overflow, 1}, {connection_port, 8082}, {connection_opts, Opts}],
    {ok, _} = gun_pool:start_pool(PoolName, Options),
    ok = gun_pool:stop_pool(PoolName),
    ok = Module:stop(),
    ok = Module:start(8080),
    ok.

gun_pool_start_unexisting_server_error_test(_Config) ->
    {ok, Module} = compile:file("../../lib/gun_pool/test/stubs/mock_http_server"),
    ok = Module:stop(),
    PoolName = test_pool,
    Options = [{pool_max_overflow, 1}, {connection_port, 8082}],
    {error, timeout} = gun_pool:start_pool(PoolName, Options),
    ok = Module:start(8080),
    ok.

gun_pool_multiple_start_test(_Config) ->
    PoolName = test_pool,
    PoolName2 = test_pool2,
    Options = [{pool_max_overflow, 1}],
    {ok, _} = gun_pool:start_pool(PoolName, Options),
    {ok, _} = gun_pool:start_pool(PoolName2, Options).

gun_pool_send_requests_test(_Config) ->
    lists:foreach(
        fun(PoolName) ->
            {ok, Status, Headers, Body} = gun_pool:send_request(PoolName, get, "/test", []),
            200 = Status,
            {<<"content-type">>, <<"text/plain">>} = lists:keyfind(<<"content-type">>, 1, Headers),
            {<<"server">>, <<"Cowboy">>} = lists:keyfind(<<"server">>, 1, Headers),
            <<"Hello, world!">> = Body
        end,
        [test_pool, test_pool2]
    ),
    ok.

gun_pool_send_request_error_test(_Config) ->
    PoolName = test_pool,
    {ok, Status, Headers, nil} = gun_pool:send_request(PoolName, get, "/test/error/noContent", []),
    204 = Status,
    false = lists:keyfind(<<"content-type">>, 1, Headers),
    {<<"server">>, <<"Cowboy">>} = lists:keyfind(<<"server">>, 1, Headers),

    Message = <<"Test message">>,
    {ok, Status1, Headers1, Body} = gun_pool:send_request(
        PoolName, post, "/test/error/inconsistentResponse", [], Message
    ),
    500 = Status1,
    <<"InconsistentResponse">> = Body,
    false = lists:keyfind(<<"content-type">>, 1, Headers1),
    {<<"server">>, <<"Cowboy">>} = lists:keyfind(<<"server">>, 1, Headers1),

    ok.

gun_pool_send_json_body_request_test(_Config) ->
    PoolName = test_pool,
    Message = #{<<"message">> => <<"Test message">>},
    {ok, Status, Headers, Body} = gun_pool:send_request(PoolName, post, "/test", [], Message),
    200 = Status,
    {<<"content-type">>, <<"application/json">>} = lists:keyfind(<<"content-type">>, 1, Headers),
    {<<"server">>, <<"Cowboy">>} = lists:keyfind(<<"server">>, 1, Headers),
    Message = Body,
    ok.

gun_pool_send_body_request_opts_test(_Config) ->
    PoolName = test_pool,
    Message = #{<<"message">> => <<"Test message">>},
    {ok, Status, Headers, Body} = gun_pool:send_request(PoolName, post, "/test", [], Message, #{
        flow => 50
    }),
    200 = Status,
    {<<"content-type">>, <<"application/json">>} = lists:keyfind(<<"content-type">>, 1, Headers),
    {<<"server">>, <<"Cowboy">>} = lists:keyfind(<<"server">>, 1, Headers),
    Message = Body,
    ok.

gun_pool_stop_test(_Config) ->
    PoolName = test_pool,
    PoolName2 = test_pool2,
    ok = gun_pool:stop_pool(PoolName),
    ok = gun_pool:stop_pool(PoolName2).
