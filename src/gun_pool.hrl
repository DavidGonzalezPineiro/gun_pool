%% @author David González Piñeiro
%% @license Apache License, Version 2.0
%% @see https://github.com/DavidGonzalezPineiro/gun_pool
%% See the LICENSE file in the project root for more information.

-type method() :: get | post | put | delete | head | options | patch.

-type gun_pool_opt() ::
    {pool_size, pos_integer()}
    | {pool_max_overflow, non_neg_integer()}
    | {connection_host, string()}
    | {connection_port, inet:port_number()}
    | {connection_opts, gun:opts()}.

-type gun_pool_opts() :: [gun_pool_opt()].
