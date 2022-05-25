-module(benchmark_test_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("eunit/include/eunit.hrl").

benchmark_testi() ->
    block_mat:first_try_benchmark(),
    erlang:display(
        block_mat:get_max_length()).

max_test() ->
    run(510).

run(600) ->
    ok;

run(N) ->
    M = utils:generateRandMat(N, N),
    Mat = numerl:matrix(M),
    C = numerl:zeros(N, N),
    {Time, _} = timer:tc(numerl, daxpy, [1.0, Mat, Mat]),
    erlang:display({time, N, Time}),
    run(N + 10).

run2(_, 0) ->
    ok;
run2(N, I) ->
    M = utils:generateRandMat(N, N),
    Mat = numerl:matrix(M),
    C = numerl:zeros(N, N),
    {Time, _} = timer:tc(numerl, daxpy, [1.0, Mat, Mat]),
    erlang:display({time, N, Time}),
    run2(N, I - 1).
