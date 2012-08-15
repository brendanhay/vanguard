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

-record(r, {id,
            status,
            chunks = [],
            pending = true}).

-define(KEY, 2).

%%
%% API
%%

empty() -> [].

insert(Id, List) ->
    case lists:keymember(Id, ?KEY, List) of
        true  -> error("Cannot reset existing id");
        false -> store(Id, #r{id = Id}, List)
    end.

set_status(Id, Status, List) ->
    case find(Id, List) of
        V when V#r.status =:= undefined -> store(Id, V#r{status = Status}, List);
        _Other                          -> error("Cannot update existing status")
    end.

add_chunk(Id, Chunk, List) ->
    Value = find(Id, List),
    store(Id, Value#r{chunks = [Chunk|Value#r.chunks]}, List).

completed(Id, List) ->
    Value = find(Id, List),
    store(Id, Value#r{pending = false}, List).

pending(List) -> length([V || V <- List, V =:= pending]).


merge([H|_]) -> {ok, 200, from_chunks(H#r.chunks)}.

%%
%% Private
%%

-spec find(_, [#r{}]) -> _.
%% @private
find(Id, List) ->
    case lists:keyfind(Id, ?KEY, List) of
        false -> error("Id does not exist");
        Value -> Value
    end.

-spec store(_, _, [#r{}]) -> _.
%% @private
store(Id, Value, List) -> lists:keystore(Id, ?KEY, List, Value).

-spec from_chunks([string()]) -> binary().
%% @private
from_chunks(Chunks) -> iolist_to_binary(Chunks).

