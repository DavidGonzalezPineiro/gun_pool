%% @author David González Piñeiro
%% @license Apache License, Version 2.0
%% @see https://github.com/DavidGonzalezPineiro/gun_pool
%% See the LICENSE file in the project root for more information.

%%%-------------------------------------------------------------------
%% @doc gun_pool public API
%% @end
%%%-------------------------------------------------------------------

-module(gun_pool).

-include("gun_pool.hrl").

-export([start_pool/1, start_pool/2, stop_pool/1, send_request/4, send_request/5, send_request/6]).

-type headers() :: [{binary(), binary()}].
-type response() :: {ok, integer(), headers(), term()} | {error, term()}.

-spec start_pool(PoolName :: atom()) -> ok | {error, term()}.
start_pool(PoolName) ->
    gun_pool_sup:start_pool(PoolName, []).

-spec start_pool(PoolName :: atom(), Opts :: gun_pool_opts()) -> ok | {error, term()}.
start_pool(PoolName, Opts) ->
    gun_pool_sup:start_pool(PoolName, Opts).

-spec stop_pool(PoolName :: atom()) -> ok | {error, term()}.
stop_pool(PoolName) ->
    gun_pool_sup:stop_pool(PoolName).

-spec send_request(
    PoolName :: atom(), Method :: method(), Path :: binary(), Headers :: headers()
) -> response().
send_request(PoolName, Method, Path, Headers) ->
    gun_pool_worker:send_request(PoolName, Method, Path, Headers).

-spec send_request(
    PoolName :: atom(),
    Method :: method(),
    Path :: binary(),
    Headers :: headers(),
    Body :: iodata() | map()
) -> response().
send_request(PoolName, Method, Path, Headers, Body) ->
    gun_pool_worker:send_request(PoolName, Method, Path, Headers, Body).

-spec send_request(
    PoolName :: atom(),
    Method :: method(),
    Path :: binary(),
    Headers :: headers(),
    Body :: iodata() | map(),
    ReqOpts :: gun:req_opts()
) -> response().
send_request(PoolName, Method, Path, Headers, Body, ReqOpts) ->
    gun_pool_worker:send_request(PoolName, Method, Path, Headers, Body, ReqOpts).
