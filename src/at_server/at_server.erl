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
-define(T_PID_POS, 3).

%% Data types
-record(trans, {t_ref :: reference(),
                t_pid :: pid()
               }).

-record(state, {user_state :: term(),
                transactions = [] :: [ #trans{} ],
                waiting = [] :: [ pid() ]
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

init([UserState]) ->
    % Trap exit messages so the AT server does not exit if a transaction
    % process dies unexpectedly
    process_flag(trap_exit, true),
    {ok, #state{ user_state = UserState }}.

%%%-------------------------------------------------------------------
%%% Call-backs handling client-side requests
%%%-------------------------------------------------------------------

%%% From at_server:stop(AT) -> {ok, UserState}
handle_call(stop, _From, State) ->
    {stop, normal, {ok, State#state.user_state}, State};

%%% From at_server:doquery(AT, Fun) -> {ok, Fun(UserState)} / error
handle_call({doquery, Fun}, _From, State) ->
    case server_query(Fun, State) of
        error -> {reply, error, State};
        Result -> {reply, {ok, Result}, State}
    end;

%%% From at_server:begin_t(AT) -> {ok, TRef}
handle_call(begin_t, _From, State) ->
    {TRef, NewState} = make_trans(State),
    {reply, {ok, TRef}, NewState};

%%% From at_server:query_t(AT, TRef, Fun) -> {ok, Fun(TransState)} / aborted
handle_call({query_t, TRef, Fun}, From, State) ->
    case find_trans_ref(TRef, State) of
        undefined -> {reply, aborted, State};
        % Query is passed off to transaction process
        Trans -> NewState = query_trans(Trans, Fun, From, State),
                 % Client call is blocked until query has finished
                 {noreply, NewState}
    end;

%%% From at_server:commit_t(AT, TRef) -> ok / aborted
handle_call({commit_t, TRef}, _From, State) ->
    case find_trans_ref(TRef, State) of
        undefined -> {reply, aborted, State};
        Trans -> NewState = commit_trans(Trans, State),
                 {noreply, NewState}
    end;

%%% Default catch-all
handle_call(_Msg, _From, State) ->
    {ok, State}.

%%% From at_server:(AT, TRef) -> ok (non-blocking)
handle_cast({update_t, TRef, Fun}, State) ->
    case find_trans_ref(TRef, State) of
        undefined -> {noreply, State};
        Trans -> update_trans(Trans, Fun),
                 {noreply, State}
    end;

%%% Default catch-all
handle_cast(_Msg, State) ->
    {noreply, State}.

%%%-------------------------------------------------------------------
%%% Call-backs handling out-of-band transaction process messages
%%%-------------------------------------------------------------------

handle_info({_TPid, {query_succeeded, Result, Client}}, State) ->
    NewState = reply_client(Client, {ok, Result}, State),
    {noreply, NewState};

handle_info({TPid, {query_failed, Client}}, State) ->
    Trans = find_trans_pid(TPid, State),
    NewState = reply_client(Client, aborted, State),
    NewState2 = abort_trans(Trans, NewState),
    {noreply, NewState2};

handle_info({_TPid, update_succeeded}, State) ->
    % Deliberate no-action
    {noreply, State};

handle_info({TPid, update_failed}, State) ->
    Trans = find_trans_pid(TPid, State),
    NewState = abort_trans(Trans, State),
    {noreply, NewState};

%%% Default catch-all
handle_info(_Reason, State) ->
    {noreply, State}.

%%% Default catch-all
terminate(_Reason, _State) ->
    ok.

%%% Default catch-all
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-------------------------------------------------------------------
%%% Utility functions
%%%-------------------------------------------------------------------

server_query(Fun, State) ->
    try
        Fun(State#state.user_state)
    catch
        _ : _ -> error
    end.

make_trans(State) ->
    % Link transaction process with AT server to ensure clean termination
    % if the server is stopped with running transactions
    {ok, TPid} = at_trans:start_link(State#state.user_state),
    TRef = make_ref(),
    Trans = #trans{ t_ref = TRef, t_pid = TPid },
    NewTransactions = [Trans | State#state.transactions],
    {TRef, State#state{ transactions = NewTransactions }}.

find_trans_ref(TRef, State) ->
    case lists:keyfind(TRef, ?T_REF_POS, State#state.transactions) of
        false -> undefined;
        Trans -> Trans
    end.

find_trans_pid(TPid, State) ->
    case lists:keyfind(TPid, ?T_PID_POS, State#state.transactions) of
        false -> undefined;
        Trans -> Trans
    end.

query_trans(Trans, Fun, Client, State) ->
    % Query is passed off to transaction process (non-blocking)
    % Process sends a message back when it is done
    at_trans:doquery(Trans#trans.t_pid, Fun, Client),
    % Add client pid to waiting list for a reply
    Waiting = [Client | State#state.waiting],
    State#state{ waiting = Waiting }.

update_trans(Trans, Fun) ->
    % Update is passed off to transaction process (non-blocking)
    % Process messages back if it fails
    at_trans:update(Trans#trans.t_pid, Fun).

commit_trans(Trans, State) ->
    {ok, NewUserState} = at_trans:queryall(Trans#trans.t_pid),
    % Abort all transactions immediately and notify waiting clients
    lists:map(fun(T) -> exit(T#trans.t_pid, abort) end, State#state.transactions),
    lists:map(fun(C) -> gen_server:reply(C, aborted) end, State#state.waiting),
    State#state{ user_state = NewUserState, transactions = [], waiting = [] }.

abort_trans(undefined, State) ->
    State;
abort_trans(Trans, State) ->
    exit(Trans#trans.t_pid, abort),
    NewTransactions = lists:delete(Trans, State#state.transactions),
    State#state{ transactions = NewTransactions }.

reply_client(Client, Msg, State) ->
    gen_server:reply(Client, Msg),
    NewWaiting = lists:delete(Client, State#state.waiting),
    State#state{ waiting = NewWaiting }.
