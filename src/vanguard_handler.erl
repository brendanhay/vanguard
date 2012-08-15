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

-include_lib("cowboy/include/http.hrl").
-include("include/vanguard.hrl").

%% Callbacks
-export([init/3,
         info/3,
         terminate/2]).

%%
%% Callbacks
%%

init({tcp, http}, Req, Backends) ->
    {ok, ProxyId} = vanguard_proxy_fsm:forward(Backends, Req),
    {loop, Req, ProxyId, ?TIMEOUT * 2, hibernate}.

info({ok, ProxyId, Status, Body}, Req, ProxyId) ->
    lager:info("REPLY ~p ~p", [ProxyId, self()]),
    {ok, NewReq} = cowboy_http_req:reply(Status, [], Body, Req),
    {ok, NewReq, ProxyId};
info(Msg, Req, ProxyId) ->
    lager:error("UNHANDLED ~p ~p ~p", [Msg, ProxyId, self()]),
    {loop, Req, ProxyId}.

terminate(Req, ProxyId) ->
    lager:info("TERMINATE ~p", [self()]),
    ok.
