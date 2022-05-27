-module(benchmark_test_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("eunit/include/eunit.hrl").

benchmark_testi() ->
    erlBlas:first_try_benchmark(),
    erlang:display(
        erlBlas:get_max_length()).

max_test() ->
    %ok.
    run(63).

run(67) ->
    ok;
run(N) ->
    M = utils:generateRandMat(N, N),
    Mat = numerl:matrix(M),
    M2 = utils:generateRandMat(N, N),
    Mat2 = numerl:matrix(M2),
    C = numerl:zeros(N, N),
    {Time, _} = timer:tc(numerl, dgemm, [1, 1, 1.5, Mat, Mat2, 2.0, C]),
    %erlang:display(
    %numerl:mtfl(C)),
    erlang:display({time, N, Time}),
    run(N + 1).

run2(_, 0) ->
    ok;
run2(N, I) ->
    M = utils:generateRandMat(N, N),
    Mat = numerl:matrix(M),
    C = numerl:zeros(N, N),
    {Time, _} = timer:tc(numerl, daxpy, [1.0, Mat, Mat]),
    erlang:display({time, N, Time}),
    run2(N, I - 1).
