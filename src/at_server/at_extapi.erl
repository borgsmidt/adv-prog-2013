%%%-------------------------------------------------------------------
%%% @doc
%%% Implementation of the atomic transaction server
%%% @end
%%%-------------------------------------------------------------------
%%% Student name: Rasmus Borgsmidt
%%% Student KU-id: qzp823
%%%-------------------------------------------------------------------

-module(at_extapi).

-export([abort/2, tryUpdate/2, ensureUpdate/2, choiceUpdate/3]).

%%%-------------------------------------------------------------------
%%% Extended API
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Aborts the specified transaction
%%
%% @spec abort(AT, Ref) -> aborted
%% where
%%   AT = pid()
%%   Ref = reference()
%% @end
%%--------------------------------------------------------------------
abort(AT, Ref) ->
    at_server:query_t(AT, Ref, fun(_) -> throw(abort) end).

%%--------------------------------------------------------------------
%% @doc
%% Tries to update the state on the specified server
%%
%% Returns: ok       if the state was updated successfully
%%          error    if the supplied function causes an error
%%          aborted  if another transaction is committed during the update
%%
%% @spec tryUpdate(AT, Fun) -> ok / error / aborted
%% where
%%   AT = pid()
%%   Fun = function(State)
%% @end
%%--------------------------------------------------------------------
tryUpdate(AT, Fun) ->
    {ok, TRef} = at_server:begin_t(AT),
    case at_server:query_t(AT, TRef, fun(S) -> S end) of
	aborted -> aborted;
	{ok, State} ->
	    try Fun(State) of
	        NewState ->
		    ok = at_server:update_t(AT, TRef, fun(_) -> NewState end),
		    at_server:commit_t(AT, TRef)
	    catch
		_ : _ -> error
	    end
    end.

%%--------------------------------------------------------------------
%% @doc
%% Tries to update the state on the specified server. This function will
%% keep trying until it succeeds, or the supplied function causes an error
%%
%% Returns: ok       if the state was updated successfully
%%          error    if the supplied function causes an error
%%
%% @spec ensureUpdate(AT, Fun) -> ok / error
%% where
%%   AT = pid()
%%   Fun = function(State)
%% @end
%%--------------------------------------------------------------------
ensureUpdate(AT, Fun) ->
    case tryUpdate(AT, Fun) of
	aborted -> ensureUpdate(AT, Fun);
	Result -> Result
    end.
	    

%%--------------------------------------------------------------------
%% @doc
%% Tries to update the state on the specified server using the supplied
%% dyadic function and list of values. It creates a separate transaction
%% for each value V, and tries to set the state on the server to Fun(State, V),
%% where State is the current state on the server.
%% 
%% Returns: {ok, NewState} if the state was updated successfully
%%          error          if the function failed for all the supplied values
%%
%% @spec choiceUpdate(AT, Fun, Values) -> {ok, NewState} / error
%% where
%%   AT = pid()
%%   Fun = function(State)
%%   Values = [term()]
%%   NewState = term()
%% @end
%%--------------------------------------------------------------------
choiceUpdate(AT, Fun, Values) ->
    ThisPid = self(),
    lists:map(fun(V) ->
		  % Create a transaction for each value in Values
                  spawn(fun() ->
		      {ok, TP} = at_server:begin_t(AT),
	              ok = at_server:update_t(AT, TP, fun(S) -> Fun(S, V) end),
	              % Must query from the transaction itself to be sure that the
                      % result is in fact what was committed, should this transaction
		      % succeed. Just reading from the server after a successful commit
                      % is not reliable, someone else could have begun/updated/committed
		      % a transaction in the meantime
		      case at_server:query_t(AT, TP, fun(S) -> S end) of
			  % The query will fail if other transactions have been committed
			  aborted -> ThisPid ! {TP, aborted};
			  {ok, Result} ->
			      case at_server:commit_t(AT, TP) of
				  aborted -> ThisPid ! {TP, aborted};
				  ok -> ThisPid ! {TP, ok, Result}
			      end
		      end
  	          end)
	      end,
	      Values),
    receive_one_ok_of(length(Values), false, result).
    

%%%-------------------------------------------------------------------
%%% Communication primitives
%%%-------------------------------------------------------------------

% Sets up a receive loop to ensure that exactly one transaction is committed
% successfully and get its result
receive_one_ok_of(0, true, Result) ->
    {ok, Result};
receive_one_ok_of(0, false, _) ->
    error;
receive_one_ok_of(StillToGo, GotOK, Result) ->
    receive
	{_TP, aborted} ->
	    receive_one_ok_of(StillToGo-1, GotOK, Result);
	{_TP, ok, NewResult} ->
	    case GotOK of
		true -> error; % Multiple OKs
		false -> receive_one_ok_of(StillToGo-1, true, NewResult)
	    end
    end.
