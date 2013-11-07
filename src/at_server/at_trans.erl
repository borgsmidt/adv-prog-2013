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
         doquery/3,
         queryall/1,
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

%% Data types
-record(state, { at_server :: pid(),
                 user_state :: term()
                }).

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
    gen_server:start_link(?MODULE, [State, self()], []).

%%--------------------------------------------------------------------
%% @doc
%% Runs the query function against the state of the transaction but is
%% non-blocking and always returns ok. The result is sent in a separate
%% message later when it is ready, and the supplied token is returned
%% with it
%%
%% @spec doquery(TP, Fun, Token) -> ok
%% where
%%   TP = pid()
%%   Fun = function(State)
%%   Token = term()
%% @end
%%--------------------------------------------------------------------
doquery(TP, Fun, Token) ->
    gen_server:cast(TP, {doquery, Fun, Token}).

%%--------------------------------------------------------------------
%% @doc
%% Returns the entire state held by this transaction process
%%
%% @spec queryall(TP) -> {ok, State}
%% where
%%   TP = pid()
%%   State = term()
%% @end
%%--------------------------------------------------------------------
queryall(TP) ->
    gen_server:call(TP, queryall).

%%--------------------------------------------------------------------
%% @doc
%% Runs the update function against the state of the transaction but is
%% non-blocking and always returns ok. A message is sent later to indicate
%% if the operation succeeded
%%
%% @spec update(TP, Fun) -> ok
%% where
%%   TP = pid()
%%   Fun = function(State)
%% @end
%%--------------------------------------------------------------------
update(TP, Fun) ->
    gen_server:cast(TP, {update, Fun}).

%%%-------------------------------------------------------------------
%%% Internal Implementation
%%%-------------------------------------------------------------------

%% gen_server callbacks

init([UserState, AT]) ->
    {ok, #state{ at_server = AT, user_state = UserState}}.

%%% From at_trans:queryall(TP) -> {ok, UserState}
handle_call(queryall, _From, State) ->
    {reply, {ok, State#state.user_state}, State};

%%% Default catch-all
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

%%% From at_trans:doquery(TP) -> ok
handle_cast({doquery, Fun, Token}, State) ->
    try Fun(State#state.user_state) of
        Result -> tell(State#state.at_server, {query_succeeded, Result, Token}),
                  {noreply, State}
    catch
        _ : _ -> tell(State#state.at_server, {query_failed, Token}),
                 {noreply, State}
    end;
    
%%% From at_trans:update(TP, Fun) -> ok
handle_cast({update, Fun}, State) ->
    try Fun(State#state.user_state) of
        NewUserState -> tell(State#state.at_server, update_succeeded),
                        NewState = State#state{ user_state = NewUserState },
                        {noreply, NewState}
    catch
        _ : _ -> tell(State#state.at_server, update_failed),
                 {noreply, State}
    end;

%%% Default catch-all
handle_cast(_Msg, State) ->
    {noreply, State}.

%%% Default catch-all
handle_info(_Reason, State) ->
    {noreply, State}.

%%% Default catch-all
terminate(_Reason, _State) ->
    ok.

%%% Default catch-all
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Utility functions

tell(Recipient, Msg) ->
    Recipient ! {self(), Msg}.
