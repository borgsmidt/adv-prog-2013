-module(at_server_tests).
-include_lib("eunit/include/eunit.hrl").

-define(STATE, [1,2,3,4,5]).
-define(REV_STATE, [5,4,3,2,1]).

%%%-------------------------------------------------------------------
%%% Tests
%%%-------------------------------------------------------------------

doquery_success_test() ->
    AT = start(),
    {ok, ?STATE} = at_server:doquery(AT, fun id/1),
    stop(AT, ?STATE),
    [].

doquery_failure_test() ->
    AT = start(),
    error = at_server:doquery(AT, fun fail/1),
    stop(AT, ?STATE),
    [].

begin_t_test() ->
    AT = start(),
    {ok, _} = at_server:begin_t(AT),
    stop(AT, ?STATE),
    [].

query_t_success_test() ->
    AT = start(),
    {ok, TP} = at_server:begin_t(AT),
    {ok, ?STATE} = at_server:query_t(AT, TP, fun id/1),
    {ok, ?STATE} = at_server:query_t(AT, TP, fun id/1),
    stop(AT, ?STATE),
    [].

query_t_failure_test() ->
    AT = start(),
    {ok, TP} = at_server:begin_t(AT),
    aborted = at_server:query_t(AT, TP, fun fail/1),
    aborted = at_server:query_t(AT, TP, fun id/1),
    stop(AT, ?STATE),
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
    stop(AT, ?STATE),
    [].

update_t_failure_test() ->
    AT = start(),
    {ok, TP} = at_server:begin_t(AT),
    {ok, ?STATE} = at_server:query_t(AT, TP, fun id/1),
    ok = at_server:update_t(AT, TP, fun fail/1),
    aborted = at_server:query_t(AT, TP, fun id/1),
    {ok, ?STATE} = at_server:doquery(AT, fun id/1),
    stop(AT, ?STATE),
    [].

commit_t_success_test() ->
    AT = start(),
    {ok, TP1} = at_server:begin_t(AT),
    {ok, TP2} = at_server:begin_t(AT),
    {ok, ?STATE} = at_server:doquery(AT, fun id/1),
    ok = at_server:update_t(AT, TP1, fun reverse/1),
    {ok, ?REV_STATE} = at_server:query_t(AT, TP1, fun id/1),
    {ok, ?STATE} = at_server:query_t(AT, TP2, fun id/1),
    ok = at_server:commit_t(AT, TP1),
    {ok, ?REV_STATE} = at_server:doquery(AT, fun id/1),
    aborted = at_server:query_t(AT, TP2, fun id/1),
    stop(AT, ?REV_STATE),
    [].

commit_t_abort_longrunning_test() ->
    AT = start(),
    {ok, TP1} = at_server:begin_t(AT),
    {ok, TP2} = at_server:begin_t(AT),
    ok = at_server:update_t(AT, TP1, fun reverse/1),
    {ok, ?REV_STATE} = at_server:query_t(AT, TP1, fun id/1),
    spawn(fun() -> timer:sleep(50), ok = at_server:commit_t(AT, TP1) end),
    % When running this test, the key is that we are not waiting 30 secs
    % for it to complete. The commit of TP1 should abort TP2 and force a return.
    % When the line below is commented out, the test fails on a timeout
    aborted = at_server:query_t(AT, TP2, fun wait30/1),
    stop(AT, ?REV_STATE),
    [].

commit_t_competing_test() ->
    AT = start(),
    TransCount = 100,
    TPs = lists:map(fun(_) -> {ok, TP} = at_server:begin_t(AT), TP end,
                    lists:seq(1, TransCount)),
    lists:map(fun(TP) -> ok = at_server:update_t(AT, TP, fun reverse/1) end, TPs),
    lists:map(fun(TP) -> {ok, ?REV_STATE} = at_server:query_t(AT, TP, fun id/1) end, TPs),
    EUnitPid = self(),
    % Attempt to commit all transactions 'simultaneously'
    lists:map(fun(TP) -> spawn(fun() -> case at_server:commit_t(AT, TP) of
                                            aborted -> EUnitPid ! {TP, aborted};
                                            ok -> EUnitPid ! {TP, ok}
                                        end
                               end)
              end, TPs),
    % Depending on how many transactions are used in this test, the following
    % call to doquery is allowed to complete with the soon-to-be outdated
    % state. This is correct behavior, when no transaction has been fully
    % committed yet. If a sufficient number of transactions are used (fx. 100),
    % the time it takes to spawn the processes is enough that the server state is
    % updated, and doquery returns the new state instead.
    {ok, _Result} = at_server:doquery(AT, fun id/1),
    % The following checks that exactly one transaction was committed successfully
    ok = receive_one_ok_of(TransCount, false),
    % When stopping the AT server, the state has always been correctly updated
    stop(AT, ?REV_STATE),
    [].


%%%-------------------------------------------------------------------
%%% Utility functions
%%%-------------------------------------------------------------------

start() ->
    {ok, AT} = at_server:start(?STATE),
    AT.

stop(AT, State) ->
    {ok, State} = at_server:stop(AT),
    ok.

id(S) -> S.

fail(_) -> throw(up).

reverse(S) -> lists:reverse(S).

wait30(S) ->
    timer:sleep(30000),
    S.

receive_one_ok_of(0, true) ->
    ok;
receive_one_ok_of(0, false) ->
    error;
receive_one_ok_of(StillToGo, GotOK) ->
    receive
        {_TP, aborted} ->
            %?debugFmt("Transaction ~p aborted", [TP]),
            receive_one_ok_of(StillToGo-1, GotOK);
        {_TP, ok} ->
            %?debugFmt("----> Transaction ~p committed", [TP]),
            case GotOK of
                true -> error; % Multiple OKs
                false -> receive_one_ok_of(StillToGo-1, true)
            end
    end.
