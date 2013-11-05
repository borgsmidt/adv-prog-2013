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
	 enquire/1,  % using 'enquire' because 'query' is a reserved word
	 update/1
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
%% Starts a new transaction process linking it to the AT server
%%
%% @spec start_link(State) -> {ok, TP}
%% where
%%   State = term()
%%   TP = pid()
%% @end
%%--------------------------------------------------------------------
start_link(State) ->
    gen_server:start_link(?MODULE, [State], []).

enquire(Fun) -> put_your_code.

update(Fun) -> put_your_code.

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
    Reply = {ok, Fun(State)},
    {reply, Reply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

