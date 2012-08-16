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
merge_properties(L, R) -> merge_properties(L, R, []).

-spec merge_properties(A, A, A) -> A.
%% @private
%% Empty right properties
merge_properties(L, [], Acc) ->
    Acc ++ L;

%% Empty left properties
merge_properties([], R, Acc) ->
    Acc ++ R;

%% Identical Key/Value
merge_properties([H|L], [H|R], Acc) ->
    merge_properties(L, R, [H|Acc]);

%% Search for the Key of Left, in Right
merge_properties([H = {K, _V}|L], R, Acc) ->
    {T, NewAcc} =
        case lists:keytake(K, 1, R) of
            {value, Prop, Rest} -> {Rest, [merge(H, Prop)|Acc]};
            false               -> {R, [H|Acc]}
        end,
    merge_properties(L, T, NewAcc).

