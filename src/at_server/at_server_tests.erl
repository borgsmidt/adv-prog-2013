-module(at_server_tests).
-include_lib("eunit/include/eunit.hrl").

-define(STATE, [1,2,3,4,5]).
-define(REV_STATE, [5,4,3,2,1]).

%%%-------------------------------------------------------------------
%%% Tests
%%%-------------------------------------------------------------------

doquery_success_test() ->
    AT = start(),
    {ok, State} = at_server:doquery(AT, fun id/1),
    stop(AT),
    [?assertEqual(?STATE, State)].

doquery_failure_test() ->
    AT = start(),
    Result = at_server:doquery(AT, fun fail/1),
    stop(AT),
    [?assertEqual(error, Result)].

begin_t_test() ->
    AT = start(),
    {ok, _} = at_server:begin_t(AT),
    stop(AT),
    [].

query_t_success_test() ->
    AT = start(),
    {ok, TP} = at_server:begin_t(AT),
    {ok, State1} = at_server:query_t(AT, TP, fun id/1),
    {ok, State2} = at_server:query_t(AT, TP, fun id/1),
    stop(AT),
    [?assertEqual(?STATE, State1), ?assertEqual(?STATE, State2)].

query_t_failure_test() ->
    AT = start(),
    {ok, TP} = at_server:begin_t(AT),
    aborted = at_server:query_t(AT, TP, fun fail/1),
    aborted = at_server:query_t(AT, TP, fun id/1),
    stop(AT),
    [].

update_t_success_test() ->
    AT = start(),
    {ok, TP1} = at_server:begin_t(AT),
    {ok, TP2} = at_server:begin_t(AT),
    {ok, ?STATE} = at_server:query_t(AT, TP1, fun id/1),
    ok = at_server:update_t(AT, TP1, fun reverse/1),
    {ok, ?REV_STATE} = at_server:query_t(AT, TP1, fun id/1),
    {ok, ?STATE} = at_server:doquery(AT, fun id/1),
    {ok, ?STATE} = at_server:query_t(AT, TP2, fun id/1),
    stop(AT),
    [].

update_t_failure_test() ->
    AT = start(),
    {ok, TP} = at_server:begin_t(AT),
    {ok, State1} = at_server:query_t(AT, TP, fun id/1),
    ok = at_server:update_t(AT, TP, fun fail/1),
    timer:sleep(50), % Sleep to give the server time to abort the transaction
    aborted = at_server:query_t(AT, TP, fun id/1),
    {ok, State2} = at_server:doquery(AT, fun id/1),
    stop(AT),
    [?assertEqual(?STATE, State1), ?assertEqual(?STATE, State2)].

commit_t_success_test() ->
    AT = start(),
    {ok, TP1} = at_server:begin_t(AT),
    {ok, TP2} = at_server:begin_t(AT),
    {ok, ?STATE} = at_server:doquery(AT, fun id/1),
    ok = at_server:update_t(AT, TP1, fun reverse/1),
    {ok, ?REV_STATE} = at_server:query_t(AT, TP1, fun id/1),
    {ok, ?STATE} = at_server:query_t(AT, TP2, fun id/1),
    ok = at_server:commit_t(AT, TP1),
%    {ok, ?REV_STATE} = at_server:doquery(AT, fun id/1),
%    aborted = at_server:query_t(AT, TP2, fun id/1),
    stop(AT),
    [].


%%%-------------------------------------------------------------------
%%% Utility functions
%%%-------------------------------------------------------------------

start() ->
    {ok, AT} = at_server:start(?STATE),
    AT.

stop(AT) ->
    {ok, ?STATE} = at_server:stop(AT),
    ok.


id(S) -> S.

fail(_) -> throw(up).

reverse(S) -> lists:reverse(S).
