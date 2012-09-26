%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(vanguard_priv_handler).

-include("include/vanguard.hrl").

%% Callbacks
-export([init/3,
         handle/2,
         terminate/2]).

%%
%% Callbacks
%%

-spec init({_, http}, _req, [backend()]) -> {ok, _req, [binary()]}.
%% @hidden
init({_Transport, http}, Req, Backends) ->
    {ok, Req, proplists:get_keys(Backends)}.

-spec handle(_req, [binary()]) -> {ok, _req, [binary()]}.
%% @hidden
handle(Req, Keys) ->
    {Status, Body} =
        case cowboy_req:path(Req) of
            {<<"/vanguard/topologies">>, _Req} ->
                {200, {<<"topologies">>, [list_to_binary(K) || K <- Keys]}};
            _Other ->
                {404, {error, <<"Not found.">>}}
        end,
    cowboy_req:reply(Status, [{<<"content-type">>, <<"application/json">>}],
                     jiffy:encode({[Body]}), Req),
    {ok, Req, Keys}.

-spec terminate(_req, reference()) -> ok.
%% @hidden
terminate(_Req, _State) -> ok.
