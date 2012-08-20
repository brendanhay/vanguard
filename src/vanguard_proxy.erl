%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(vanguard_proxy).

-behaviour(gen_fsm).

-include_lib("cowboy/include/http.hrl").
-include("include/vanguard.hrl").

%% API
-export([start_link/4,
         forward/2]).

%% Callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

%% States
-export([execute/2,
         wait/2,
         merge/2]).

%%
%% Macros
%%

-define(TIMEOUT_MSG, request_timeout).
-define(HEADERS,     ibrowse_async_headers).
-define(CHUNK,       ibrowse_async_response).
-define(END,         ibrowse_async_response_end).

%%
%% Types
%%

-record(s, {proxy_id :: reference(),
            from     :: pid(),
            backends :: [backend()],
            req      :: #http_req{},
            replies  :: vanguard_replies:replies()}).

-type ibrowse_id()       :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
-type ibrowse_response() :: {?HEADERS, ibrowse_id(), string(), [any()]} |
                            {?CHUNK, ibrowse_id(), string()} |
                            {?END, ibrowse_id()}.

%%
%% API
%%

-spec start_link(reference(), pid(), [backend()], #http_req{})
    -> {ok, pid()} | ignore | {error, _}.
%% @doc
start_link(ProxyId, From, Backends, Req) ->
    gen_fsm:start_link(?MODULE, #s{proxy_id = ProxyId,
                                   from     = From,
                                   backends = Backends,
                                   req      = Req}, []).

-spec forward([backend()], #http_req{}) -> {ok, reference()}.
%% @doc
forward(Backends, Req) ->
    ProxyId = make_ref(),
    {ok, _Pid} = vanguard_proxy_sup:start_child([ProxyId, self(), Backends, Req]),
    {ok, ProxyId}.

%%
%% Callbacks
%%

-spec init(#s{}) -> {ok, execute, #s{}, 0}.
%% @hidden
init(State) ->
    lager:info("PROXY-FSM-INIT"),
    {ok, execute, State, 0}.

-spec handle_event(_, atom(), #s{}) -> {stop, tuple(), #s{}}.
%% @hidden
handle_event(Event, _StateName, State) ->
    {stop, {unhandled_event, Event}, State}.

-spec handle_sync_event(_, pid(), atom(), #s{}) -> {stop, tuple(), #s{}}.
%% @hidden
handle_sync_event(Event, _From, _StateName, State) ->
    {stop, {unhandled_sync_event, Event}, State}.

-spec handle_info(_, atom(), #s{}) -> {stop, normal, #s{}} | {next_state, atom(), #s{}, 0}.
%% @hidden
handle_info(?TIMEOUT_MSG, _StateName, State = #s{proxy_id = ProxyId, from = From}) ->
    From ! {timeout, ProxyId},
    {stop, normal, State};
handle_info(Msg, _StateName, State) ->
    ?MODULE:wait(Msg, State).

-spec terminate(_, atom(), #s{}) -> ok.
%% @hidden
terminate(_Reason, _StateName, _State) -> ok.

-spec code_change(_, atom(), #s{}, _) -> {ok, atom(), #s{}}.
%% @hidden
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%
%% States
%%

-spec execute(timeout, #s{}) -> {next_state, wait, #s{}}.
%% @hidden
execute(timeout, State = #s{backends = Backends, req = Req}) ->
    _TRef = schedule_timeout(),
    {ok, Replies} = multi_request(Backends, Req),
    {next_state, wait, State#s{replies = Replies}}.

-spec wait(ibrowse_response(), #s{}) -> {next_state, wait | merge, #s{}}.
%% @hidden
wait({?HEADERS, ReplyId, Status, _Headers}, State) ->
    F = vanguard_replies:set_status(ReplyId, Status, _),
    {next_state, wait, update_replies(F, State)};
wait({?CHUNK, ReplyId, Content}, State) ->
    lager:info("CHUNK ~p", [ReplyId]),
    F = vanguard_replies:add_chunk(ReplyId, Content, _),
    {next_state, wait, update_replies(F, State)};
wait({?END, ReplyId}, State) ->
    lager:info("END ~p", [ReplyId]),
    F = vanguard_replies:completed(ReplyId, _),
    NewState = update_replies(F, State),
    case vanguard_replies:pending(NewState#s.replies) of
        0  -> {next_state, merge, NewState, 0};
        _N -> {next_state, wait, NewState}
    end.

-spec merge(timeout, #s{}) -> {stop, normal, #s{}}.
%% @hidden
merge(timeout, State = #s{proxy_id = ProxyId, from = From, replies = Replies}) ->
    {ok, Status, Merged} = vanguard_replies:result(Replies),
    From ! {ok, ProxyId, Status, Merged},
    {stop, normal, State}.

%%
%% Private
%%

-spec update_replies(fun((vanguard_replies:replies()) -> vanguard_replies:replies()), #s{})
    -> #s{}.
%% @private
update_replies(F, State = #s{replies = Replies}) -> State#s{replies = F(Replies)}.

-spec schedule_timeout() -> timer:tref().
%% @private
schedule_timeout() -> erlang:send_after(?TIMEOUT, self(), ?TIMEOUT_MSG).

-spec multi_request([string()], #http_req{}) -> {ok, vanguard_replies:replies()}.
%% @private
multi_request(Backends, Req) ->
    multi_request(Backends, Req, vanguard_replies:empty()).

-spec multi_request([backend()], #http_req{}, vanguard_replies:replies())
    -> {ok, vanguard_replies:replies()}.
%% @private
multi_request([], _Req, Replies) ->
    {ok, Replies};
multi_request([H | T], Req, Replies) ->
    NewReplies =
        case request(H, Req) of
            {ibrowse_req_id, Id} ->
                vanguard_replies:insert(Id, Replies);
            Error ->
                lager:error("REQ ~s ~p", [H, Error]),
                Replies
        end,
    multi_request(T, Req, NewReplies).

-spec request(backend(), #http_req{}) -> any().
%% @private
request(Backend, Req) ->
    Uri = uri(Backend, Req),
    lager:info("~s ~s ~p", [Req#http_req.method, Uri, self()]),
    ibrowse:send_req(Uri, [], method(Req), [], [{stream_to, self()}]).

-spec uri(backend(), #http_req{}) -> string().
%% @private
uri(Backend, #http_req{raw_path = Path, raw_qs = Query}) ->
    Base = string:join([Backend, tl(binary_to_list(Path))], "/"),
    case Query of
        <<>>  -> Base;
        _Else -> string:join([Base, binary_to_list(Query)], "?")
    end.

-spec method(#http_req{}) -> get | post.
%% @private
method(#http_req{method = Method}) ->
    list_to_atom(string:to_lower(atom_to_list(Method))).


