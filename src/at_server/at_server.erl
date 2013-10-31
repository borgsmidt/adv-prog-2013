%%%-------------------------------------------------------------------
%%% @author Michael Kirkedal Thomsen <shapper@diku.dk>
%%% @copyright (C) 2013, Michael Kirkedal Thomsen
%%% @doc
%%% Skeleton for AP Exam 2013.
%%% Implementation of the atomic transaction server
%%% @end
%%% Created : Oct 2013 by Michael Kirkedal Thomsen <shapper@diku.dk>
%%%-------------------------------------------------------------------
%%% Student name:
%%% Student KU-id:
%%%-------------------------------------------------------------------

-module(at_server).

-export([start/1, stop/1, begin_t/1, doquery/2, query_t/3, update_t/3, commit_t/2]).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

start(State) ->
    put_your_code.

stop(AT) -> put_your_code.

doquery(AT, Fun) -> put_your_code.

% Returns a reference
begin_t(AT) -> put_your_code.

query_t(AT, Ref, Fun) -> put_your_code.

update_t(AT, Ref, Fun) -> put_your_code.

commit_t(AT, Ref) -> put_your_code.

%%%-------------------------------------------------------------------
%%% Communication primitives
%%%-------------------------------------------------------------------

%% synchronous communication

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} -> Response
    end.

reply(From,  Msg) ->
    From ! {self(), Msg}.

reply_ok(From) ->
    reply(From, ok).

reply_ok(From, Msg) ->
    reply(From, {ok, Msg}).

reply_error(From, Msg) ->
    reply(From, error).

reply_abort(From) ->
    reply(From, aborted).

%% asynchronous communication

info(Pid, Msg) ->
    Pid ! Msg.

%%%-------------------------------------------------------------------
%%% Internal Implementation
%%%-------------------------------------------------------------------

% Your implementation of the atomic transaction server.

