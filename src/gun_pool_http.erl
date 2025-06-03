%% @author David González Piñeiro
%% @license Apache License, Version 2.0
%% @see https://github.com/DavidGonzalezPineiro/gun_pool
%% See the LICENSE file in the project root for more information.

%%%-------------------------------------------------------------------
%% @doc gun_pool http module.
%% @end
%%%-------------------------------------------------------------------

-module(gun_pool_http).

-include("gun_pool.hrl").

%% API
-export([open_connection/3, send_request/4, send_request/5, send_request/6]).

%%%===================================================================
%%% API functions
%%%===================================================================
-spec open_connection(inet:hostname() | inet:ip_address(), inet:port_number(), gun:opts()) ->
    {ok, pid()} | {error, term()}.
open_connection(Host, Port, Opts) ->
    logger:info("[~p] Opening connection to ~p:~p", [?MODULE, Host, Port]),
    case gun:open(Host, Port, Opts) of
        {ok, ConnPid} ->
            case gun:await_up(ConnPid) of
                {ok, Protocol} when Protocol == http; Protocol == http2 ->
                    {ok, ConnPid};
                {error, Reason} ->
                    logger:error("[~p] Connection established but failed to upgrade: ~p", [
                        ?MODULE, Reason
                    ]),
                    {error, Reason}
            end;
        {error, Reason} ->
            logger:error("[~p] Failed to open connection: ~p", [?MODULE, Reason]),
            {error, Reason}
    end.

-spec send_request(pid(), method(), binary(), [{binary(), binary()}]) ->
    {ok, integer(), list(), term()} | {error, term()}.
send_request(ConnPid, Method, Path, Headers) ->
    StreamRef = gun:request(ConnPid, atom_to_binary(Method, utf8), Path, Headers, <<>>),
    receive_response(ConnPid, StreamRef).

-spec send_request(pid(), method(), binary(), [{binary(), binary()}], iodata() | map()) ->
    {ok, integer(), list(), term()} | {error, term()}.
send_request(ConnPid, Method, Path, Headers, Body) ->
    StreamRef = gun:request(
        ConnPid, atom_to_binary(Method, utf8), Path, Headers, encode_body(Body)
    ),
    receive_response(ConnPid, StreamRef).

-spec send_request(
    pid(), method(), binary(), [{binary(), binary()}], iodata() | map(), gun:req_opts()
) ->
    {ok, integer(), list(), term()} | {error, term()}.
send_request(ConnPid, Method, Path, Headers, Body, ReqOpts) ->
    StreamRef = gun:request(
        ConnPid, atom_to_binary(Method, utf8), Path, Headers, encode_body(Body), ReqOpts
    ),
    receive_response(ConnPid, StreamRef).

%%%===================================================================
%%% Internal functions
%%%===================================================================
encode_body(Body) when is_map(Body) ->
    jiffy:encode(Body);
encode_body(Body) ->
    Body.

receive_response(ConnPid, StreamRef) ->
    case gun:await(ConnPid, StreamRef) of
        {response, fin, Status, Headers} ->
            {ok, Status, Headers, nil};
        {response, nofin, Status, Headers} ->
            {ok, Body} = gun:await_body(ConnPid, StreamRef),
            DecodedBody =
                case is_json_response(Headers) of
                    true ->
                        try
                            jiffy:decode(Body, [return_maps])
                        catch
                            _:Reason ->
                                logger:warning("[~p] Failed to decode JSON: ~pBody: ~p", [
                                    ?MODULE, Reason, Body
                                ]),
                                Body
                        end;
                    false ->
                        Body
                end,
            {ok, Status, Headers, DecodedBody};
        {error, Reason} ->
            logger:error("[~p] Error in response: ~p", [?MODULE, Reason]),
            {error, Reason}
    end.

is_json_response(Headers) ->
    case lists:keyfind(<<"content-type">>, 1, Headers) of
        {_, ContentType} ->
            binary:match(ContentType, <<"+json">>) =/= nomatch orelse
                binary:match(ContentType, <<"application/json">>) =/= nomatch;
        false ->
            false
    end.
