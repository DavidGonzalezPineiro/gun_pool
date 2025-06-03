%% @author David González Piñeiro
%% @license Apache License, Version 2.0
%% @see https://github.com/DavidGonzalezPineiro/gun_pool
%% See the LICENSE file in the project root for more information.

-module(mock_http_server).
-behaviour(cowboy_handler).

-export([start/1, start_tls/1, stop/0]).
-export([init/2]).

start(Port) ->
    Dispatch = make_dispatch(),
    {ok, _Pid} = cowboy:start_clear(?MODULE, [{port, Port}], #{
        env => #{dispatch => Dispatch}
    }),
    ct:pal("Mock HTTP server started on port ~p", [Port]),
    ok.

start_tls(Port) ->
    Dispatch = make_dispatch(),
    {ok, _Pid} = cowboy:start_tls(?MODULE, [
        {port, Port},
        {certfile, "../../lib/gun_pool/test/config/ssl/cert.pem"},
        {keyfile, "../../lib/gun_pool/test/config/ssl/key.pem"}
    ], #{
        env => #{dispatch => Dispatch},
        protocols => [http2]
    }),
    ct:pal("Mock HTTPS server started on port ~p", [Port]),
    ok.

stop() ->
    ok = cowboy:stop_listener(?MODULE),
    ct:pal("Mock HTTP server stopped"),
    ok.

init(Req0, _Opts) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    case {Method, Path} of
        {<<"post">>, <<"/test">>} ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Body,
                Req1),
            {ok, Req, undefined};
        {<<"get">>, <<"/test/error/noContent">>} ->
            Req = cowboy_req:reply(204,
                #{},
                <<>>,
                Req0),
            {ok, Req, undefined};
        {<<"post">>, <<"/test/error/inconsistentResponse">>} ->
            Body = <<"InconsistentResponse">>,
            Req = cowboy_req:reply(500,
                #{},
                Body,
                Req0),
            {ok, Req, undefined};
        _ ->
            Body = <<"Hello, world!">>,
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"text/plain">>},
                Body,
                Req0),
            {ok, Req, undefined}
    end.


%%-------------------------------------------------------------------
make_dispatch() ->
    cowboy_router:compile([
        {'_', [
            {"/", ?MODULE, []},
            {"/test", ?MODULE, []},
            {"/test/error/noContent", ?MODULE, []},
            {"/test/error/inconsistentResponse", ?MODULE, []}
        ]}
    ]).
