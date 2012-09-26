%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(vanguard_config).

-include("include/vanguard.hrl").

%% API
-export([ip/0,
         port/0,
         acceptors/0,
         backends/0,
         option/2]).

%%
%% API
%%

-spec ip() -> inet:ip_address().
%% @doc
ip() -> {0, 0, 0, 0}.

-spec port() -> inet:port_number().
%% @doc
port() -> list_to_integer(env(port)).

-spec acceptors() -> pos_integer().
%% @doc
acceptors() -> list_to_integer(env(acceptors)).

-spec backends() -> [backend()].
%% @doc
backends() -> parse_backends(string:tokens(os("BACKENDS"), ","), []).

-spec option(ip | atom(), options()) ->  inet:ip_address() | any().
%% @doc
option(ip, Opts) ->
    {ok, Ip} = inet:getaddr(lookup_option(ip, Opts), inet),
    Ip;
option(Key, Opts) ->
    lookup_option(Key, Opts).

%%
%% Private
%%

-spec env(atom()) -> any().
%% @doc
env(Key) ->
    application:load(myxi),
    case application:get_env(myxi, Key) of
        {ok, Value} when is_atom(Value) -> os(atom_to_list(Value));
        {ok, Value}                     -> Value;
        undefined                       -> error({config_not_set, Key})
    end.

-spec os(string()) -> string().
%% @doc
os(Key) ->
    case os:getenv(Key) of
        false -> error({env_not_set, Key});
        Env   -> Env
    end.

-spec lookup_option(atom(), options()) -> any().
%% @private
lookup_option(Key, Opts) ->
    {Key, Value} = lists:keyfind(Key, 1, Opts),
    Value.

-spec parse_backends([string()], [backend()]) -> [backend()].
%% @private
parse_backends([], Acc) ->
    Acc;
parse_backends([Pair|T], Acc) ->
    [K, V] = string:tokens(Pair, "="),
    All = case lists:keyfind(K, 1, Acc) of
              false     -> [];
              {K, List} -> List
          end,
    parse_backends(T, lists:keystore(K, 1, Acc, {K, [V|All]})).
