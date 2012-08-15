%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(vanguard_json).

-include("include/vanguard.hrl").

%% API
-export([merge/1]).

%%
%% Macros
%%

%%
%% Types
%%

%%
%% API
%%

-spec merge([A]) -> A.
%% @doc
merge(Chunks) -> lists:foldl(merge(_, _), [], Chunks).

%%
%% Private
%%

%% JSON            Erlang
%%
%% null           -> null
%% true           -> true
%% false          -> false
%% [104, 105]     -> [104, 105]
%% "hi"           -> <<"hi">>
%% "hi"           -> <<"hi">>
%% 1              -> 1
%% 1.25           -> 1.25
%% []             -> []
%% [true, 1.0]    -> [true, 1.0]
%% {}             -> {[]}
%% {"foo": "bar"} -> {[{<<"foo">>, <<"bar">>}]}
%% {"foo": "bar"} -> {[{<<"foo">>, <<"bar">>}]}

-spec merge(A, A) -> A.
%% @private
%% Two Lists
merge(A, B) when is_list(A) andalso is_list(B) ->
    lists:usort(A ++ B);
%% Object Keys
merge({Key, A}, {Key, B}) ->
    {Key, merge(A, B)};
%% Whatevers, Bro
merge(A, A) -> A.
