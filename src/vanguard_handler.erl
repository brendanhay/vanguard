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

-define(TIMEOUT, 5000).

%%
%% Callbacks
%%

init({tcp, http}, Req, Opts) ->
    {loop, Req, undefined_state, ?TIMEOUT, hibernate}.

info({reply, Body}, Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(200, [], Body, Req),
    {ok, Req2, State};
info(Message, Req, State) ->
    {loop, Req, State, hibernate}.

terminate(Req, State) ->
    ok.
