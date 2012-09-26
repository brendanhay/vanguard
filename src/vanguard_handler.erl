%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(vanguard_handler).

-include("include/vanguard.hrl").

%% Callbacks
-export([init/3,
         info/3,
         terminate/2]).

%%
%% Callbacks
%%

-spec init({_, http}, _req, [backend()])
    -> {loop, _req, reference(), non_neg_integer(), hibernate}.
%% @hidden
init({_Transport, http}, Req, Backends) ->
    X = case cowboy_req:cookie(?COOKIE, Req) of
            undefined ->
                false;
            {undefined, _Req} ->
                false;
            {Key, _Req} ->
                lager:info("COOKIE ~p", [Key]),
                lists:keyfind(binary_to_list(Key), 1, Backends)
          end,
    Y = case X of
            false        -> lists:concat([U || {_, U} <- Backends]);
            {_Key, Uris} -> Uris
        end,
    lager:info("FORWARD ~p from ~p", [Y, Backends]),
    {ok, ProxyId} = vanguard_proxy:forward(Y, Req),
    {loop, Req, ProxyId, ?TIMEOUT * 2, hibernate}.

-spec info(_, _req, reference()) -> {ok | loop, _req, reference()}.
%% @hidden
info({ok, ProxyId, Status, Body}, Req, ProxyId) ->
    lager:info("REPLY ~p ~p", [ProxyId, self()]),
    {ok, NewReq} = cowboy_req:reply(Status, [], Body, Req),
    {ok, NewReq, ProxyId};
info({timeout, ProxyId}, Req, ProxyId) ->
    {ok, NewReq} = cowboy_req:reply(204, [], <<>>, Req),
    {ok, NewReq, ProxyId};
info(Msg, Req, ProxyId) ->
    lager:error("UNHANDLED ~p ~p ~p", [Msg, ProxyId, self()]),
    {loop, Req, ProxyId}.

-spec terminate(_req, reference()) -> ok.
%% @hidden
terminate(_Req, _ProxyId) ->
    lager:info("TERMINATE ~p", [self()]),
    ok.
