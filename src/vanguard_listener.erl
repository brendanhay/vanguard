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

-spec start_link() -> {ok, pid()}.
%% @doc
start_link() -> {ok, listener()}.

%%
%% Private
%%

-spec listener() -> pid().
%% @private
listener() ->
    Acceptors = vanguard_config:acceptors(),
    Tcp       = [{ip, vanguard_config:ip()}, {port, vanguard_config:port()}],
    Dispatch  = [{dispatch, routes()}],
    case cowboy:start_listener(http_listener, Acceptors,
                               cowboy_tcp_transport, Tcp,
                               cowboy_http_protocol, Dispatch) of
        {ok, Pid} ->
            lager:info("LISTEN ~s", [vanguard_net:format_ip(Tcp)]),
            link(Pid),
            Pid;
        Error ->
            lager:error("LISTENER failed to start: ~p", [Error]),
            exit(listener_start_failure)
    end.

-spec routes() -> [_].
%% @private
routes() ->
    Backends = vanguard_config:backends(),
    [{'_', [
        %% / -> ./priv/www/index.html
        static([], [{file, <<"index.html">>}]),

        %% /api -> ./priv/www/api/index.html
        static([<<"api">>], [{file, <<"api/index.html">>}]),

        %% /api/* requests
        {[<<"api">>, '...'], vanguard_handler, Backends},

        %% Static files under ./priv/www
        static(['...'])
    ]}].

-spec static([binary() | atom()]) -> {}.
%% @private
static(Match) -> static(Match, []).

-spec static([binary() | atom()], options()) -> {}.
%% @private
static(Match, Opts) ->
    {Match, cowboy_http_static,
        [{directory, {priv_dir, vanguard, [<<"www">>]}},
         {mimetypes, {fun mimetypes:path_to_mimes/2, default}},
         {etag, {attributes, [filepath, filesize, inode, mtime]}}|
         Opts]}.
