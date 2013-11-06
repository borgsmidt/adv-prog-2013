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
-define(T_REF_POS, 2).

%% Data types
-record(trans, {t_ref :: reference(),
		t_pid :: pid()
	       }).

-record(state, {user_state :: term(),
		transactions = [] :: [ #trans{} ]
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

query_t(AT, Ref, Fun) ->
    % Avoid timing out at this point, handled by the transaction process
    gen_server:call(AT, {query_t, Ref, Fun}, infinity).

update_t(AT, Ref, Fun) ->
    gen_server:cast(AT, {update_t, Ref, Fun}).

commit_t(AT, Ref) ->
    gen_server:call(AT, {commit_t, Ref}).

%%%-------------------------------------------------------------------
%%% Internal Implementation
%%%-------------------------------------------------------------------

%% gen_server callbacks

init([UserState]) ->
    % Trap exit messages so the AT server does not exit if a transaction
    % process dies unexpectedly
    process_flag(trap_exit, true),
    {ok, #state{ user_state = UserState }}.

handle_call(stop, _From, State) ->
    {stop, normal, {ok, State}, State#state.user_state};
handle_call({doquery, Fun}, _From, State) ->
    try Fun(State#state.user_state) of
        Result -> {reply, {ok, Result}, State}
    catch
        _ : _ -> {reply, error, State}
    end;
handle_call(begin_t, _From, State) ->
    {TRef, NewState} = make_trans(State),
    {reply, {ok, TRef}, NewState};
handle_call({query_t, TRef, Fun}, _From, State) ->
    Trans = find_trans(TRef, State),
    io:format("All: ~p~n", [State#state.transactions]),
    io:format("Trans: ~p~n", [Trans]),
    case query_trans(Trans, Fun) of
        undefined -> {reply, aborted, State};
        error -> NewState = abort_trans(Trans, State),
                 {reply, aborted, NewState};
        Reply -> {reply, Reply, State}
    end;
handle_call(_Msg, _From, State) ->
    {ok, State}.

handle_cast({update_t, TRef, Fun}, State) ->
    Trans = find_trans(TRef, State),
    io:format("All: ~p~n", [State#state.transactions]),
    io:format("Trans: ~p~n", [Trans]),
    case update_trans(Trans, Fun) of
	error -> NewState = abort_trans(Trans, State),
		 {noreply, NewState};
	_ -> {noreply, State}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Utility functions

make_trans(State) ->
    % Link transaction process with AT server to ensure clean termination
    % if the server is stopped with running transactions
    {ok, TPid} = at_trans:start_link(State#state.user_state),
    TRef = make_ref(),
    Trans = #trans{ t_ref = TRef, t_pid = TPid },
    NewTransactions = [Trans | State#state.transactions],
    {TRef, State#state{ transactions = NewTransactions }}.

abort_trans(Trans, State) ->
    exit(Trans#trans.t_pid, abort),
    NewTransactions = lists:keydelete(Trans#trans.t_ref, ?T_REF_POS,
				      State#state.transactions),
    State#state{ transactions = NewTransactions }.

find_trans(TRef, State) ->
    case lists:keyfind(TRef, ?T_REF_POS, State#state.transactions) of
	false -> undefined;
	Trans -> Trans
    end.

query_trans(undefined, _) ->
    undefined;
query_trans(Trans, Fun) ->
    at_trans:doquery(Trans#trans.t_pid, Fun).
	
update_trans(undefined, _) ->
    undefined;
update_trans(Trans, Fun) ->
    at_trans:update(Trans#trans.t_pid, Fun).
