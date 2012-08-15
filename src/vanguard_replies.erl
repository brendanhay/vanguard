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

%% API
-export([empty/0,
         insert/2,
         set_status/3,
         add_chunk/3,
         completed/2,
         pending/1,
         merge/1]).

%%
%% Types
%%

-type id() :: term()

-record(r, {id             :: term(),
            status         :: undefined | string(),
            chunks = []    :: [string()],
            pending = true :: boolean()}).

-opaque replies() :: [#r{}].

-export_types([replies/0]).

%%
%% Macros
%%

-define(KEY, 2).

%%
%% API
%%

-spec empty() -> replies().
%% @public
empty() -> [].

-spec insert(id(), replies()) -> replies().
%% @public
insert(Id, List) ->
    case lists:keymember(Id, ?KEY, List) of
        true  -> error("Cannot reset existing id");
        false -> store(Id, #r{id = Id}, List)
    end.

-spec set_status(id(), non_neg_integer(), replies()) -> replies().
%% @public
set_status(Id, Status, List) ->
    case find(Id, List) of
        V when V#r.status =:= undefined -> store(Id, V#r{status = Status}, List);
        _Other                          -> error("Cannot update existing status")
    end.

-spec add_chunk(id(), string(), replies()) -> replies().
%% @public
add_chunk(Id, Chunk, List) ->
    Value = find(Id, List),
    store(Id, Value#r{chunks = [Chunk|Value#r.chunks]}, List).

-spec completed(id(), replies()) -> replies().
%% @public
completed(Id, List) ->
    Value = find(Id, List),
    store(Id, Value#r{pending = false}, List).

-spec pending(replies()) -> non_neg_integer().
%% @public
pending(List) -> length([V || V <- List, V =:= pending]).

-spec merge(replies()) -> error | {ok, 200, binary()}.
%% @public
merge([])    -> error;
merge([H|_]) -> {ok, 200, from_chunks(H)}.

%%
%% Private
%%

-spec find(id(), replies()) -> replies().
%% @private
find(Id, List) ->
    case lists:keyfind(Id, ?KEY, List) of
        false -> error("Id does not exist");
        Value -> Value
    end.

-spec store(id(), term(), replies()) -> replies().
%% @private
store(Id, Value, List) -> lists:keystore(Id, ?KEY, List, Value).

-spec from_chunks(#r{}) -> binary().
%% @private
from_chunks(#r{chunks = Chunks}) -> iolist_to_binary(Chunks).
