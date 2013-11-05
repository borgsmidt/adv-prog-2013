%%%-------------------------------------------------------------------
%%% @doc
%%% Implementation of the transaction process for the atomic transaction server
%%% @end
%%%-------------------------------------------------------------------
%%% Student name: Rasmus Borgsmidt
%%% Student KU-id: qzp823
%%%-------------------------------------------------------------------

-module(at_trans).

-behavior(gen_server).

%% API exports
-export([
	 start_link/1,
	 stop/1,
	 doquery/2,
	 update/2
	]).

%% gen_server callbacks
-export([
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%% Macros
-define(DEFAULT_TIMEOUT, 5000).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts a new transaction process, linking it to the AT server
%%
%% @spec start_link(State) -> {ok, TP}
%% where
%%   State = term()
%%   TP = pid()
%% @end
%%--------------------------------------------------------------------
start_link(State) ->
    gen_server:start_link(?MODULE, [State], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the specified transaction process
%%
%% @spec stop(TP) -> ok
%% where
%%   TP = pid()
%% @end
%%--------------------------------------------------------------------
stop(TP) ->
    gen_server:call(TP, stop).

%%--------------------------------------------------------------------
%% @doc
%% Runs the query function against the state of the transaction and
%% returns the result
%%
%% The atom 'error' is returned, if the supplied query function fails
%% or if the call times out
%%
%% @spec doquery(TP, Fun) -> {ok, Result} or error
%% where
%%   TP = pid()
%%   Fun = function(State)
%%   Result = term()
%% @end
%%--------------------------------------------------------------------
doquery(TP, Fun) ->
    % We are allowing the client-provided function a 'reasonable' amount
    % of time to complete its call, although we cannot really know how
    % long it needs. But if we use 'infinity', we expose our transaction
    % process to the risk of being stalled indefinitely by a rogue function
    try
        gen_server:call(TP, {doquery, Fun}, ?DEFAULT_TIMEOUT)
    catch
	_ : _ -> error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Runs the update function against the state of the transaction
%%
%% The atom 'error' is returned, if the supplied update function fails
%% or if the call times out
%%
%% @spec update(TP, Fun) -> ok or error
%% where
%%   TP = pid()
%%   Fun = function(State)
%% @end
%%--------------------------------------------------------------------
update(TP, Fun) ->
    % We are allowing the client-provided function a 'reasonable' amount
    % of time to complete its call, although we cannot really know how
    % long it needs. But if we use 'infinity', we expose our transaction
    % process to the risk of being stalled indefinitely by a rogue function
    try
        gen_server:call(TP, {update, Fun}, ?DEFAULT_TIMEOUT)
    catch
	_ : _ -> error
    end.

%%%-------------------------------------------------------------------
%%% Internal Implementation
%%%-------------------------------------------------------------------

%% gen_server callbacks

init([State]) ->
    {ok, State}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call({doquery, Fun}, _From, State) ->
    try Fun(State) of
	Result -> {reply, {ok, Result}, State}
    catch
	_ : _ -> {reply, error, State}
    end;
handle_call({update, Fun}, _From, State) ->
    try Fun(State) of
	NewState -> {reply, ok, NewState}
    catch
	_ : _ -> {reply, error, State}
    end;
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

