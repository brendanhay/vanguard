%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(vanguard_proxy_fsm).

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
%% Types
%%

-record(s, {proxy_id :: reference(),
            from     :: pid(),
            backends :: [backend()],
            req      :: #http_req{},
            replies  :: vanguard_replies:replies()}).

-define(TIMEOUT_MSG, request_timeout).
-define(HEADERS,     ibrowse_async_headers).
-define(CHUNK,       ibrowse_async_response).
-define(END,         ibrowse_async_response_end).

%%
%% API
%%

%%-spec start_link() -> {ok, pid()} | ignore | {error, _}.
%% @doc
start_link(ProxyId, From, Backends, Req) ->
    gen_fsm:start_link(?MODULE, #s{proxy_id = ProxyId,
                                   from     = From,
                                   backends = Backends,
                                   req      = Req}, []).

%% @doc
forward(Backends, Req) ->
    ProxyId = make_ref(),
    vanguard_proxy_sup:start_child([ProxyId, self(), Backends, Req]),
    {ok, ProxyId}.

%%
%% Callbacks
%%

-spec init(#s{}) -> {ok, route, #s{}}.
%% @hidden
init(State) ->
    lager:info("PROXY-FSM-INIT"),
    {ok, execute, State, 0}.

%% @hidden
handle_event(Event, _StateName, State) ->
    {stop, {unhandled_event, Event}, State}.

%% @hidden
handle_sync_event(Event, _From, _StateName, State) ->
    {stop, {unhandled_sync_event, Event}, State}.

%% @hidden
handle_info(?TIMEOUT_MSG, _StateName,
            State = #s{proxy_id = ProxyId, from = From}) ->
    From ! {timeout, ProxyId},
    {stop, normal, State};
handle_info(Msg, _StateName, State) ->
    ?MODULE:wait(Msg, State).

%% @hidden
terminate(_Reason, _StateName, _State) -> ok.

%% @hidden
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%
%% States
%%

%% @hidden
execute(timeout, State = #s{backends = Backends, req = Req}) ->
    _TRef = schedule_timeout(),
    {ok, Replies} = multi_request(Backends, Req),
    {next_state, wait, State#s{replies = Replies}}.

%% @hidden
wait({?HEADERS, ReplyId, Status, _Headers}, State) ->
    F = vanguard_replies:set_status(ReplyId, Status, _),
    {next_state, wait, update_replies(F, State)};
wait({?CHUNK, ReplyId, Content}, State) ->
    F = vanguard_replies:add_chunk(ReplyId, Content, _),
    {next_state, wait, update_replies(F, State)};
wait({?END, ReplyId}, State) ->
    F = vanguard_replies:completed(ReplyId, _),
    NewState = update_replies(F, State),
    Next     = case vanguard_replies:pending(NewState#s.replies) of
                   0  -> merge;
                   _N -> wait
               end,
    {next_state, Next, NewState, 0}.

%% @hidden
merge(timeout, State = #s{proxy_id = ProxyId, from = From, replies = Replies}) ->
    {ok, Status, Merged} = vanguard_replies:merge(Replies),
    From ! {ok, ProxyId, Status, Merged},
    {stop, normal, State}.

%%
%% Private
%%

-spec update_replies(fun((vanguard_replies:replies()) -> vanguard_replies:replies()), #s{})
    -> #s{}.
%% @private
update_replies(F, State = #s{replies = Replies}) -> State#s{replies = F(Replies)}.

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
multi_request([H|T], Req, Replies) ->
    {ibrowse_req_id, Id} = request(H, Req),
    multi_request(T, Req, vanguard_replies:insert(Id, Replies)).

-spec request(backend(), #http_req{}) -> {ibrowse_req_id, term()}.
%% @private
request(Backend, Req) ->
    Uri = uri(Backend, Req),
    lager:info("~s ~s ~p", [Req#http_req.method, Uri, self()]),
    ibrowse:send_req(Uri, [], method(Req), [], [{stream_to, self()}]).

-spec uri(backend(), #http_req{}) -> string().
%% @private
uri(Backend, #http_req{raw_path = Path, raw_qs = Query}) ->
    Base = vanguard_config:option(uri, Backend),
    string:join([Base, tl(binary_to_list(Path)), binary_to_list(Query)], "/").

-spec method(#http_req{}) -> get | post.
%% @private
method(#http_req{method = Method}) ->
    list_to_atom(string:to_lower(atom_to_list(Method))).
