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
-define(DEFAULT_TIMEOUT, 5000).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts a new Atomic Transaction (AT) server
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
%% Stops the specified AT server
%%
%% @spec stop(AT) -> {ok, State}
%% where
%%   AT = pid()
%%   State = term()
%% @end
%%--------------------------------------------------------------------
stop(AT) ->
    gen_server:call(AT, stop).

%%--------------------------------------------------------------------
%% @doc
%% Runs the specified query function against the current state of the
%% AT server
%%
%% @spec doquery(AT, Fun) -> {ok, Result} or error
%% where
%%   AT = pid()
%%   Fun = function(State)
%%   Result = term()
%% @end
%%--------------------------------------------------------------------
doquery(AT, Fun) ->
    % We are allowing the client-provided function a 'reasonable' amount
    % of time to complete its call, although we cannot really know how
    % long it needs. But if we use 'infinity', we expose our AT server
    % to the risk of being stalled indefinitely by a rogue query function
    try
        gen_server:call(AT, {doquery, Fun}, ?DEFAULT_TIMEOUT)
    catch
	_ : _ -> error
    end.

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
    {stop, normal, Reply, State};
handle_call({doquery, Fun}, _From, State) ->
    % Leave failure handling to doquery
    Reply = {ok, Fun(State)}
    {reply, Reply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Reason, State) ->
    {noreply, State}.

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
