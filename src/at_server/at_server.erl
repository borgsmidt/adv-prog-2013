%%%-------------------------------------------------------------------
%%% @author Michael Kirkedal Thomsen <shapper@diku.dk>
%%% @copyright (C) 2013, Michael Kirkedal Thomsen
%%% @doc
%%% Skeleton for AP Exam 2013.
%%% Implementation of the atomic transaction server
%%% @end
%%% Created : Oct 2013 by Michael Kirkedal Thomsen <shapper@diku.dk>
%%%-------------------------------------------------------------------
%%% Student name: Rasmus Borgsmidt
%%% Student KU-id: qzp823
%%%-------------------------------------------------------------------

-module(at_server).

-behavior(gen_server).

%% API exports
-export([
	 start/1,
	 stop/1,
	 begin_t/1,
	 doquery/2,
	 query_t/3,
	 update_t/3,
	 commit_t/2
	]).

%% gen_server callbacks
-export([
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%% Definitions
-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts a new Atomic Transaction Server
%%
%% @spec start(State) -> {ok, AT}
%% where
%%   State = term()
%%   AT = pid()
%% @end
%%--------------------------------------------------------------------
start(State) ->
    gen_server:start(?MODULE, [State], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the specified Atomic Transaction Server
%%
%% @spec stop(AT) -> {ok, State}
%% where
%%   AT = pid()
%%   State = term()
%% @end
%%--------------------------------------------------------------------
stop(AT) ->
    gen_server:call(AT, stop).

doquery(AT, Fun) -> put_your_code.

% Returns a reference
begin_t(AT) -> put_your_code.

query_t(AT, Ref, Fun) -> put_your_code.

update_t(AT, Ref, Fun) -> put_your_code.

commit_t(AT, Ref) -> put_your_code.

%%%-------------------------------------------------------------------
%%% Internal Implementation
%%%-------------------------------------------------------------------

%% gen_server callbacks

init([State]) ->
    {ok, State}.

handle_call(stop, _From, State) ->
    Reply = {ok, State},
    {stop, normal, Reply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

% Your implementation of the atomic transaction server.



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
