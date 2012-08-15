%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(vanguard_sup).

-behaviour(supervisor).

-include("include/vanguard.hrl").

%% API
-export([start_link/0]).

%% Callbacks
-export([init/1]).

%%
%% API
%%

-spec start_link() -> {ok, pid()} | ignore | {error, _}.
%% @doc
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%
%% Callbacks
%%

-spec init([]) -> {ok, {{one_for_all, 3, 20}, [supervisor:child_spec()]}}.
%% @hidden
init([]) ->
    %% Ensure the listeners are linked against the supervisor process
    {ok, _Pids} = vanguard_listener:start_link(),
    FsmSup = {vanguard_proxy_sup,
              {vanguard_proxy_sup, start_link, []},
              permanent, 5000, supervisor, [vanguard_proxy_sup]},
    {ok, {{one_for_one, 5, 10}, [FsmSup]}}.
