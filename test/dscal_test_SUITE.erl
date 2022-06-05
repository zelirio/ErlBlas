-module(dscal_test_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("eunit/include/eunit.hrl").

base_test() ->
    A = [[1]],
    B = [[2]],
    ABlock = erlBlas:matrix(A),
    BBlock = erlBlas:matrix(B),
    erlBlas:dscal(2.0, ABlock),
    ?assert(erlBlas:equals(ABlock, BBlock)).

max_size_blocks_test() ->
    A = [
        [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
        [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
        [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
        [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
        [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
        [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
        [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
        [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
        [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
        [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    ],

    B = [
        [2, 4, 6, 8, 10, 12, 14, 16, 18, 20],
        [2, 4, 6, 8, 10, 12, 14, 16, 18, 20],
        [2, 4, 6, 8, 10, 12, 14, 16, 18, 20],
        [2, 4, 6, 8, 10, 12, 14, 16, 18, 20],
        [2, 4, 6, 8, 10, 12, 14, 16, 18, 20],
        [2, 4, 6, 8, 10, 12, 14, 16, 18, 20],
        [2, 4, 6, 8, 10, 12, 14, 16, 18, 20],
        [2, 4, 6, 8, 10, 12, 14, 16, 18, 20],
        [2, 4, 6, 8, 10, 12, 14, 16, 18, 20],
        [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]
    ],

    ABlock = erlBlas:matrix(A),
    BBlock = erlBlas:matrix(B),
    erlBlas:dscal(2.0, ABlock),
    ?assert(erlBlas:equals(ABlock, BBlock)).

float_test() ->
    A = [[1.2, 2.5, 3.6, 4.7, 5.69, 42.69], [6.24, 7.77, 8.42, 9.58, 10.013, 69.42]],

    ABlock = erlBlas:matrix(A),
    erlBlas:dscal(2.3, ABlock),
    ANum = numerl:matrix(A),
    numerl:dscal(2.3, ANum),
    Expected = numerl:mtfl(ANum),
    Actual = erlBlas:toErl(ABlock),
    ?assert(mat:'=='(Expected, Actual)).

corner_cases_test_() ->
    erlBlas:set_max_length(50),
    Sizes = [49, 50, 51, 99, 100, 101],
    {timeout, 100, fun() -> matrix_test_core(Sizes) end}.

random_test() ->
    erlBlas:set_max_length(50),
    Sizes = [rand:uniform(800) + 500, rand:uniform(800) + 500, rand:uniform(800) + 500],
    {timeout, 100, fun() -> matrix_test_core(Sizes) end}.

matrix_test_core(Sizes) ->
    lists:map(
        fun(M) ->
            lists:map(
                fun(N) ->
                    random_test_core(M, N),
                    random_positive_test_core(M, N),
                    random_negative_test_core(M, N)
                end,
                Sizes
            )
        end,
        Sizes
    ).

random_test_core(M, N) ->
    A = utils:generateRandMat(M, N),
    ABlock = erlBlas:matrix(A),
    erlBlas:dscal(1.0, ABlock),
    Expected = A,
    Actual = erlBlas:toErl(ABlock),
    ?assert(mat:'=='(Expected, Actual)).

random_negative_test_core(M, N) ->
    A = utils:generateRandMat(M, N),
    ABlock = erlBlas:matrix(A),
    erlBlas:dscal(-2.5, ABlock),
    ANum = numerl:matrix(A),
    numerl:dscal(-2.5, ANum),
    Expected = numerl:mtfl(ANum),
    Actual = erlBlas:toErl(ABlock),
    ?assert(mat:'=='(Expected, Actual)).

random_positive_test_core(M, N) ->
    A = utils:generateRandMat(M, N),
    ABlock = erlBlas:matrix(A),
    erlBlas:dscal(2.5, ABlock),
    ANum = numerl:matrix(A),
    numerl:dscal(2.5, ANum),
    Expected = numerl:mtfl(ANum),
    Actual = erlBlas:toErl(ABlock),
    ?assert(mat:'=='(Expected, Actual)).
