%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(vanguard_json_tests).

-include("include/vanguard_test.hrl").

-define(MOD, vanguard_json).

%%
%% Properties
%%

numeric_values_are_summed_test() ->
    ?EQC(?FORALL({L, R}, {number(), number()},
                 begin
                     (L + R) =:= merge(L, R)
                 end)).

object_keys_are_merged_test() ->
    ?EQC(?FORALL({L, R}, {json_object(), json_object()},
                 begin
                     lists:usort(all_keys(L) ++ all_keys(R))
                         =:= all_keys(merge(L, R))
                 end)).

lists_are_concatenated_and_uniq_test() ->
    ?EQC(?FORALL({L, R}, {values(), values()},
                 begin
                     lists:usort(L ++ R) =:= merge(L, R)
                 end)).

%%
%% Generators
%%

json_object() -> {list({key(), value()})}.

key() -> non_empty(binary()).

all_keys({[]})    -> [];
all_keys({Props}) -> lists:sort(proplists:get_keys(Props)).

values() -> non_empty(list(value())).

value() ->
    union([null,
           boolean(),
           string(),
           safe_binary(),
           number()]).

safe_binary() ->
    ?LET(B, non_empty(binary()), << <<C>> || <<C>> <= B, (C > 0) and (C =< 127) >>).

%%
%% Helpers
%%

merge(L, R) -> vanguard_json:merge(L, R).
