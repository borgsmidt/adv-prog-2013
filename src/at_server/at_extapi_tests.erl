-module(at_extapi_tests).
-include_lib("eunit/include/eunit.hrl").

-define(STATE, [1,2,3,4,5]).
-define(REV_STATE, [5,4,3,2,1]).

%%%-------------------------------------------------------------------
%%% Tests
%%%-------------------------------------------------------------------

abort_test() ->
    AT = start(),
    {ok, TP} = at_server:begin_t(AT),
    aborted = at_extapi:abort(AT, TP),
    stop(AT, ?STATE),
    [].

try_update_error_test() ->
    AT = start(),
    error = at_extapi:tryUpdate(AT, fun fail/1),
    stop(AT, ?STATE),
    [].

try_update_aborted_test() ->
    AT = start(),
    {ok, TP} = at_server:begin_t(AT),
    ok = at_server:update_t(AT, TP, fun reverse/1),
    EUnitPid = self(),
    spawn(fun() -> EUnitPid ! at_extapi:tryUpdate(AT, fun(S) -> timer:sleep(100),
								lists:reverse(S)
						      end)
	  end),
    timer:sleep(50),
    ok = at_server:commit_t(AT, TP),
    receive
	Msg -> ?assertEqual(aborted, Msg)
    end,
    stop(AT, ?REV_STATE),
    [].

try_update_ok_test() ->
    AT = start(),
    ok = at_extapi:tryUpdate(AT, fun reverse/1),
    stop(AT, ?REV_STATE),
    [].

ensure_update_error_test() ->
    AT = start(),
    error = at_extapi:ensureUpdate(AT, fun fail/1),
    stop(AT, ?STATE),
    [].

ensure_update_ok_test() ->
    AT = start(),
    ok = at_extapi:ensureUpdate(AT, fun reverse/1),
    stop(AT, ?REV_STATE),
    [].

ensure_update_retry_test() ->
    AT = start(),
    {ok, TP} = at_server:begin_t(AT),
    ok = at_server:update_t(AT, TP, fun reverse/1),
    EUnitPid = self(),
    spawn(fun() -> EUnitPid ! at_extapi:ensureUpdate(AT, fun(S) -> timer:sleep(100),
								   lists:reverse(S)
							 end)
	  end),
    timer:sleep(50),
    ok = at_server:commit_t(AT, TP),
    receive
	Msg -> ?assertEqual(ok, Msg)
    end,
    stop(AT, ?STATE),
    [].

choice_update_test() ->
    AT = start(),
    {ok, State} = at_extapi:choiceUpdate(AT, fun(S, V) -> lists:map(fun(A) -> A+V end, S) end,
					 [2,3,5,7,11,13]),
    stop(AT, State),
    [].

%%%-------------------------------------------------------------------
%%% Utility functions
%%%-------------------------------------------------------------------

start() ->
    {ok, AT} = at_server:start(?STATE),
    AT.

stop(AT, Expected) ->
    {ok, State} = at_server:stop(AT),
    ?assertEqual(Expected, State),
    ok.

fail(_) -> throw(up).

reverse(S) -> lists:reverse(S).
