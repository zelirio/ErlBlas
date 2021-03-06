-module(benchmark_test_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("eunit/include/eunit.hrl").

benchmark_test() ->
    erlang:display(erlBlas:get_max_length()).

max_test() ->
    {timeout, 1000, run(40, 20, [])}.

run(80, 0, _) ->
    ok;
run(N, 0, Acc) ->
    erlang:display(N),
    erlang:display(stats(Acc)),
    run(N + 1, 40, []);
run(N, T, Acc) ->
    M = utils:generateRandMat(N, N),
    Mat = numerl:matrix(M),
    M2 = utils:generateRandMat(N, N),
    Mat2 = numerl:matrix(M2),
    C = numerl:zeros(N, N),
    {Time, _} = timer:tc(numerl, dgemm, [1, 1, 1.5, Mat, Mat2, 2.0, C]),
    run(N, T - 1, [Time | Acc]).

stats(List) ->
    case List of
        [X] ->
            {X, 0};
        _ ->
            {mean(List), var(List, mean(List))}
    end.

mean(List) ->
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
