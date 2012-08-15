%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(vanguard_proxy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_child/1]).

%% Callbacks
-export([init/1]).

%%
%% API
%%

-spec start_link() -> {ok, pid()} | ignore | {error, _}.
%% @doc
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_child(_) -> {ok, pid()}.
%% @private
start_child(Args) -> supervisor:start_child(?MODULE, Args).

%%
%% Callbacks
%%

-spec init([]) -> {ok, {{simple_one_for_one, 10, 10}, [supervisor2:child_spec()]}}.
%% @hidden
init([]) ->
    Proxy = {vanguard_proxy,
             {vanguard_proxy, start_link, []},
             temporary, 5000, worker, [vanguard_proxy]},
    {ok, {{simple_one_for_one, 10, 10}, [Proxy]}}.
