%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(vanguard_listener).

-include("include/vanguard.hrl").

%% API
-export([start_link/0]).

%%
%% API
%%

-spec start_link() -> ok.
%% @doc
start_link() ->
    [link(P) || {ok, P} <- [listener(C) || C <- vanguard_config:env(listeners)]],
    ok.

%%
%% Private
%%

-spec listener(listener()) -> {ok, pid()}.
%% @private
listener(Config) ->
    Acceptors = vanguard_config:option(acceptors, Config),
    Tcp = tcp_options(Config),
    Dispatch = [{dispatch, routes()}],
    case cowboy:start_listener(http_listener, Acceptors,
                               cowboy_tcp_transport, Tcp,
                               cowboy_http_protocol, Dispatch) of
        {ok, Pid} ->
            lager:info("LISTEN ~s", [vanguard_net:format_ip(Tcp)]),
            {ok, Pid};
        Error ->
            lager:error("LISTENER failed to start: ~p", [Error]),
            exit(listener_start_failure)
    end.

-spec tcp_options(listener()) -> [proplists:property()].
%% @private
tcp_options(Config) ->
    [{ip, vanguard_config:option(ip, Config)},
     {port, vanguard_config:option(port, Config)}|vanguard_config:env(tcp)].

-spec routes() -> [_].
%% @private
routes() -> [{'_', [{'_', vanguard_handler, []}]}].
