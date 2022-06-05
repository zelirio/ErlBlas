-module(dgemm_perf_test_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(mat, ['*'/2, '*´'/2, tr/1, '+'/2, '-'/2, '=='/2, eval/1]).

performance_test_() ->
    {timeout, 100, fun() ->
        performance(),
        performance_conc()
    end}.

performance_conc() ->
    timer:sleep(100),
    %dgemm_conc_exec_time(10,100),
    Sizes = [10, 50, 100, 200, 350, 500],
    Results =
        lists:map(
            fun(Size) ->
                Times = dgemm_conc_exec_time(10, Size),
                stats(Times)
            end,
            Sizes
        ),
    erlang:display({conc, Results}).

dgemm_conc_exec_time(N, Size) ->
    M1 = utils:generateRandMat(Size, Size),
    M2 = utils:generateRandMat(Size, Size),
    M3 = utils:generateRandMat(Size, Size),
    Mat1 = erlBlas:matrix(M1),
    Mat2 = erlBlas:matrix(M2),
    Mat3 = erlBlas:matrix(M3),
    {Time, _} = timer:tc(erlBlas, dgemm, [false, false, 1.0, Mat1, Mat2, 1.0, Mat3]),
    if
        N == 1 ->
            [Time];
        true ->
            [Time | dgemm_conc_exec_time(N - 1, Size)]
    end.

performance() ->
    timer:sleep(100),
    %dgemm_exec_time(10,100),
    Sizes = [10, 50, 100, 200, 350, 500],
    Results =
        lists:map(
            fun(Size) ->
                Times = dgemm_exec_time(10, Size),
                stats(Times)
            end,
            Sizes
        ),
    erlang:display({seq, Results}).

dgemm_exec_time(N, Size) ->
    M1 = utils:generateRandMat(Size, Size),
    M2 = utils:generateRandMat(Size, Size),
    M3 = utils:generateRandMat(Size, Size),
    Mat1 = erlBlas:matrix(M1),
    Mat2 = erlBlas:matrix(M2),
    Mat3 = erlBlas:matrix(M3),
    {Time, _} = timer:tc(sequential, dgemm, [false, false, 1.0, Mat1, Mat2, 1.0, Mat3]),
    if
        N == 1 ->
            [Time];
        true ->
            [Time | dgemm_exec_time(N - 1, Size)]
    end.

stats(List) ->
    case List of
        [X] ->
            {X, 0};
        _ ->
            {mean(List), var(List, mean(List))}
    end.

mean(List) ->
    %erlang:display(List),
    lineSum(List) / length(List).

var(List, Mean) ->
    lineSum(lists:map(fun(Elem) -> (Elem - Mean) * (Elem - Mean) end, List)) / length(List).

lineSum([H | T]) ->
    lineSum(T, H).

lineSum(List, Acc) ->
    case List of
        [H | T] ->
            lineSum(T, Acc + H);
        [] ->
            Acc
    end.
