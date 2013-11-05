%%%-------------------------------------------------------------------
%%% @doc
%%% Implementation of the atomic transaction (AT) server
%%% @end
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
	 code_change/3
	]).

%% Macros
-define(DEFAULT_TIMEOUT, 5000).

%% Data types
-record(state, {user_state :: term(),
		running_ts = [] :: [ { reference(), pid() } ]
	       }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts a new AT server
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
%% AT server and returns the result
%%
%% The atom 'error' is returned, if the supplied query function fails
%% or if the call times out
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

%%--------------------------------------------------------------------
%% @doc
%% Begins a transaction on the current state of the AT server and returns
%% a transaction reference
%%
%% @spec begin_t(AT) -> {ok, Ref}
%% where
%%   AT = pid()
%%   Ref = reference()
%% @end
%%--------------------------------------------------------------------
begin_t(AT) ->
    gen_server:call(AT, begin_t).

query_t(AT, Ref, Fun) -> put_your_code.

update_t(AT, Ref, Fun) -> put_your_code.

commit_t(AT, Ref) -> put_your_code.

%%%-------------------------------------------------------------------
%%% Internal Implementation
%%%-------------------------------------------------------------------

%% gen_server callbacks

init([UserState]) ->
    {ok, #state{ user_state = UserState }}.

handle_call(stop, _From, State) ->
    Reply = {ok, State},
    {stop, normal, Reply, State};
handle_call({doquery, Fun}, _From, State) ->
    try Fun(State#state.user_state) of
	Result -> {reply, {ok, Result}, State}
    catch
	_ : _ -> {reply, error, State}
    end;
handle_call(begin_t, _From, State) ->
    UserState = State#state.user_state,
    % Link transaction process with AT server to ensure clean termination
    % if the server is stopped with running transactions
    {ok, TPid} = at_trans:start_link(UserState),
    TRef = make_ref(),
    RunningTs = State#state.running_ts,
    NewState = State#state{ running_ts = [{TRef, TPid} | RunningTs] },
    {reply, {ok, TRef}, NewState}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
