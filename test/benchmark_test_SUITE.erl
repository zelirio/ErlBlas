-module(benchmark_test_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("eunit/include/eunit.hrl").

benchmark_test() ->
    block_mat:first_try_benchmark(),
    erlang:display(
        block_mat:get_max_length()).

max_test() ->
    ok.

    %run2(64, 200).

run(81) ->
    ok;
run(N) ->
    M = utils:generateRandMat(N, N),
    Mat = numerl:matrix(M),
    C = numerl:zeros(N, N),
    {Time, _} = timer:tc(numerl, dgemm, [1, 1, 2.5, Mat, Mat, 3.5, C]),
    erlang:display({time, N, Time}),
    run(N + 1).

run2(_, 0) ->
    ok;
run2(N, I) ->
    M = utils:generateRandMat(N, N),
    Mat = numerl:matrix(M),
    C = numerl:zeros(N, N),
    {Time, _} = timer:tc(numerl, dgemm, [1, 1, 2.5, Mat, Mat, 3.5, C]),
    erlang:display({time, N, Time}),
    run2(N, I - 1).
