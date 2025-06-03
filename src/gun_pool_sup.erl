%% @author David González Piñeiro
%% @license Apache License, Version 2.0
%% @see https://github.com/DavidGonzalezPineiro/gun_pool
%% See the LICENSE file in the project root for more information.

%%%-------------------------------------------------------------------
%% @doc gun_pool top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(gun_pool_sup).

-behaviour(supervisor).

-include("gun_pool.hrl").

-export([start_link/0]).
-export([init/1, start_pool/2, stop_pool/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },
    {ok, {SupFlags, []}}.

start_pool(PoolName, Opts) ->
    DefaultConfig = [
        {pool_size, application:get_env(gun_pool, pool_size, 20)},
        {pool_max_overflow, application:get_env(gun_pool, pool_max_overflow, 5)},
        {connection_host, application:get_env(gun_pool, connection_host, "localhost")},
        {connection_port, application:get_env(gun_pool, connection_port, 8080)},
        {connection_opts, application:get_env(gun_pool, connection_opts, #{})}
    ],

    Config = merge_opts(Opts, DefaultConfig),

    PoolSize = proplists:get_value(pool_size, Config),
    MaxOverflow = proplists:get_value(pool_max_overflow, Config),
    Host = proplists:get_value(connection_host, Config),
    Port = proplists:get_value(connection_port, Config),
    ConnOpts = proplists:get_value(connection_opts, Config),

    ChildSpec =
        #{
            id => PoolName,
            start =>
                {poolboy, start_link, [
                    [
                        {name, {local, PoolName}},
                        {worker_module, gun_pool_worker},
                        {size, PoolSize},
                        {max_overflow, MaxOverflow}
                    ],
                    [
                        {pool_name, PoolName},
                        {host, Host},
                        {port, Port},
                        {opts, ConnOpts}
                    ]
                ]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [poolboy]
        },
    filter_poolboy_res(supervisor:start_child(?SERVER, ChildSpec)).

stop_pool(PoolName) ->
    supervisor:terminate_child(?SERVER, PoolName),
    supervisor:delete_child(?SERVER, PoolName).

merge_opts(Primary, Default) ->
    Keys = [K || {K, _} <- Primary],
    Remaining = [KV || {K, _} = KV <- Default, not lists:member(K, Keys)],
    Primary ++ Remaining.

filter_poolboy_res({error, Reason} = Result) ->
    case extract_poolboy_badmatch(Reason) of
        {ok, Badmatch} -> Badmatch;
        error -> Result
    end;
filter_poolboy_res(Other) ->
    Other.

extract_poolboy_badmatch({{{badmatch, Badmatch}, Stack}, _ChildSpec}) ->
    case
        lists:any(
            fun
                ({poolboy, _, _, _}) -> true;
                (_) -> false
            end,
            Stack
        )
    of
        true -> {ok, Badmatch};
        false -> error
    end;
extract_poolboy_badmatch(_) ->
    error.
