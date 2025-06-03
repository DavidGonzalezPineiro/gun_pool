%% @author David González Piñeiro
%% @license Apache License, Version 2.0
%% @see https://github.com/DavidGonzalezPineiro/gun_pool
%% See the LICENSE file in the project root for more information.

%%%-------------------------------------------------------------------
%% @doc gun_pool worker module.
%% @end
%%%-------------------------------------------------------------------

-module(gun_pool_worker).

-behaviour(gen_server).

-include("gun_pool.hrl").

%% API
-export([start_link/1, send_request/4, send_request/5, send_request/6]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {conn, pool_name}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

-spec send_request(atom(), method(), binary(), [{binary(), binary()}]) ->
    {ok, integer(), list(), term()} | {error, term()}.
send_request(PoolName, Method, Path, Headers) ->
    poolboy_transaction(PoolName, {send_request, Method, Path, Headers}).

-spec send_request(atom(), method(), binary(), [{binary(), binary()}], iodata() | map()) ->
    {ok, integer(), list(), term()} | {error, term()}.
send_request(PoolName, Method, Path, Headers, Body) ->
    poolboy_transaction(PoolName, {send_request, Method, Path, Headers, Body}).

-spec send_request(
    atom(), method(), binary(), [{binary(), binary()}], iodata() | map(), gun:req_opts()
) ->
    {ok, integer(), list(), term()} | {error, term()}.
send_request(PoolName, Method, Path, Headers, Body, ReqOpts) ->
    poolboy_transaction(PoolName, {send_request, Method, Path, Headers, Body, ReqOpts}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(Args) ->
    PoolName = proplists:get_value(pool_name, Args),
    Host = proplists:get_value(host, Args),
    Port = proplists:get_value(port, Args),
    Opts = proplists:get_value(opts, Args),
    case gun_pool_http:open_connection(Host, Port, Opts) of
        {ok, ConnPid} ->
            {ok, #state{conn = ConnPid, pool_name = PoolName}};
        {error, Reason} ->
            {error, Reason}
    end.

handle_call(
    {send_request, Method, Path, Headers},
    _From,
    #state{conn = ConnPid} =
        State
) ->
    Response = gun_pool_http:send_request(ConnPid, Method, Path, Headers),
    {reply, Response, State};
handle_call(
    {send_request, Method, Path, Headers, Body},
    _From,
    #state{conn = ConnPid} =
        State
) ->
    Response = gun_pool_http:send_request(ConnPid, Method, Path, Headers, Body),
    {reply, Response, State};
handle_call(
    {send_request, Method, Path, Headers, Body, ReqOpts},
    _From,
    #state{conn = ConnPid} =
        State
) ->
    Response = gun_pool_http:send_request(ConnPid, Method, Path, Headers, Body, ReqOpts),
    {reply, Response, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, #state{conn = ConnPid}) ->
    gun:close(ConnPid),
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

poolboy_transaction(PoolName, Request) ->
    poolboy:transaction(
        PoolName,
        fun(Worker) -> gen_server:call(Worker, Request) end
    ).
