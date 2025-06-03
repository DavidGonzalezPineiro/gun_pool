%% @author David González Piñeiro
%% @license Apache License, Version 2.0
%% @see https://github.com/DavidGonzalezPineiro/gun_pool
%% See the LICENSE file in the project root for more information.

%%%-------------------------------------------------------------------
%% @doc gun_pool_app
%% @end
%%%-------------------------------------------------------------------

-module(gun_pool_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    gun_pool_sup:start_link().

stop(_State) ->
    ok.
