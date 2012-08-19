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
-export([merge/2]).

%%
%% API
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
%% Numbers
merge(L, R) when is_number(L), is_number(R) -> L + R;

%% Rabbit's representation of empty objects, + usual arrays
merge([], R) -> R;
merge(L, []) -> L;

%% Lists
merge(L, R) when is_list(L), is_list(R) -> lists:usort(L ++ R);

%% Objects
merge({L}, {R}) -> {merge_properties(L, R)};

%% Object Key/Values
merge({Key, A}, {Key, B}) -> {Key, merge(A, B)};

%% %% Comparison
merge(L, L) -> L;

%% Dev
merge(L, _R) -> L.

%%
%% Private
%%

-spec merge_properties(A, A) -> A.
%% @private
%% Properties
merge_properties([], R) ->
    R;
merge_properties([{K, LV}|L], R) ->
    Value = case lists:keyfind(K, 1, R) of
                {K, RV} -> merge(LV, RV);
                false   -> LV
            end,
    merge_properties(L, lists:keystore(K, 1, R, {K, Value})).
