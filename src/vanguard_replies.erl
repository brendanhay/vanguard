%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(vanguard_replies).

-include("include/vanguard.hrl").

%% API
-export([empty/0,
         insert/2,
         set_status/3,
         add_chunk/3,
         completed/2,
         pending/1,
         result/1]).

%%
%% Macros
%%

-define(KEY, 2).

%%
%% Types
%%

-type id() :: term().

-record(r, {id             :: term(),
            status         :: undefined | pos_integer(),
            chunks = []    :: [string()],
            pending = true :: boolean()}).

-opaque replies() :: [#r{}].

-export_types([replies/0]).

%%
%% API
%%

-spec empty() -> replies().
%% @doc
empty() -> [].

-spec insert(id(), replies()) -> replies().
%% @doc
insert(Id, Replies) ->
    case lists:keymember(Id, ?KEY, Replies) of
        true  -> error("Cannot reset existing id");
        false -> store(Id, #r{id = Id}, Replies)
    end.

-spec set_status(id(), string() | pos_integer(), replies()) -> replies().
%% @doc
set_status(Id, Status, Replies) when is_list(Status) ->
    set_status(Id, list_to_integer(Status), Replies);
set_status(Id, Status, Replies) ->
    case find(Id, Replies) of
        V when V#r.status =:= undefined -> store(Id, V#r{status = Status}, Replies);
        _Other                          -> error("Cannot update existing status")
    end.

-spec add_chunk(id(), string(), replies()) -> replies().
%% @doc
add_chunk(Id, Chunk, Replies) ->
    Value = find(Id, Replies),
    store(Id, Value#r{chunks = [Chunk|Value#r.chunks]}, Replies).

-spec completed(id(), replies()) -> replies().
%% @doc
completed(Id, Replies) ->
    Value = find(Id, Replies),
    store(Id, Value#r{pending = false}, Replies).

-spec pending(replies()) -> non_neg_integer().
%% @doc
pending(Replies) -> length([R || R <- Replies, R#r.pending]).

-spec result(replies()) -> {ok, pos_integer(), [binary()]}.
%% @doc
result(Replies) ->
    {Status, NewReplies} = aggr(Replies),
    Merged = lists:foldl(vanguard_json:merge(_, _), [], from_json(NewReplies)),
    {ok, Status, jiffy:encode(Merged)}.

%%
%% Private
%%

-spec find(id(), replies()) -> tuple().
%% @private
find(Id, Replies) ->
    case lists:keyfind(Id, ?KEY, Replies) of
        false -> error("Id does not exist");
        Value -> Value
    end.

-spec store(id(), term(), replies()) -> replies().
%% @private
store(Id, Value, Replies) -> lists:keystore(Id, ?KEY, Replies, Value).

-spec aggr(replies()) -> {pos_integer(), [_]}.
%% @private
aggr([])      -> {204, []};
aggr(Replies) -> aggr(Replies, {[], []}).

-spec aggr(replies(), {[_], [_]}) -> {pos_integer(), [_]}.
%% @private
aggr([], {S, E}) when length(S) =:= 0 ->
    {500, E};
aggr([], {S, _E}) ->
    {200, S};
aggr([R = #r{status = Status} | T], Acc = {S, E}) ->
    aggr(T, case Status of
                200    -> {[R | S], E};
                500    -> {S, [R | E]};
                _Other -> Acc
            end).

-spec from_json(replies()) -> [term()].
%% @private
from_json(Replies) ->
    [jiffy:decode(C) || C <- from_chunks(Replies), C =/= <<>>].

-spec from_chunks(replies) -> [binary()].
%% @private
from_chunks(Replies) ->
    [iolist_to_binary(C) || #r{chunks = C} <- Replies, C =/= []].
