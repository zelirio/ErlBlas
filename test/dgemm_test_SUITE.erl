-module(dgemm_test_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(mat, ['*'/2, '*´'/2, tr/1, '+'/2, '-'/2, '=='/2, eval/1]).
-import(erlBlas, [dgemm/7]).

base_test() ->
    A = [[1]],
    B = [[1]],
    C = [[1]],
    R = [[2]],
    ABlock = erlBlas:matrix(A),
    BBlock = erlBlas:matrix(B),
    CBlock = erlBlas:matrix(C),
    RBlock = erlBlas:matrix(R),
    erlBlas:dgemm(false, true, 1.0, ABlock, BBlock, 1.0, CBlock),
    ?assert(erlBlas:equals(RBlock, CBlock)).

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

    C = [
        [110, 220, 330, 440, 550, 660, 770, 880, 990, 1100],
        [110, 220, 330, 440, 550, 660, 770, 880, 990, 1100],
        [110, 220, 330, 440, 550, 660, 770, 880, 990, 1100],
        [110, 220, 330, 440, 550, 660, 770, 880, 990, 1100],
        [110, 220, 330, 440, 550, 660, 770, 880, 990, 1100],
        [110, 220, 330, 440, 550, 660, 770, 880, 990, 1100],
        [110, 220, 330, 440, 550, 660, 770, 880, 990, 1100],
        [110, 220, 330, 440, 550, 660, 770, 880, 990, 1100],
        [110, 220, 330, 440, 550, 660, 770, 880, 990, 1100],
        [110, 220, 330, 440, 550, 660, 770, 880, 990, 1100]
    ],

    ABlock = erlBlas:matrix(A),
    BBlock = erlBlas:matrix(B),
    CBlock = erlBlas:zeros(10, 10),
    RBlock = erlBlas:matrix(C),
    erlBlas:dgemm(false, false, 1, ABlock, BBlock, 0, CBlock),
    ?assert(erlBlas:equals(RBlock, CBlock)).

float_test() ->
    A = [[-1.2, 2.5, 3.6, 4.7, 5.69, 42.69], [6.24, 7.77, 8.42, -9.58, 10.013, 69.42]],

    B = [
        [1.2, 2.5],
        [3.6, 4.7],
        [-5.69, 42.69],
        [6.24, -7.77],
        [8.42, 9.58],
        [-10.013, 69.42]
    ],

    ABlock = erlBlas:matrix(A),
    BBlock = erlBlas:matrix(B),
    CBlock = erlBlas:zeros(2, 2),
    erlBlas:dgemm(false, false, 1, ABlock, BBlock, 0, CBlock),
    ANum = numerl:matrix(A),
    BNum = numerl:matrix(B),
    CNum = numerl:zeros(2, 2),
    numerl:dgemm(0, 0, 1, ANum, BNum, 0, CNum),
    Expected = numerl:mtfl(CNum),
    Actual = erlBlas:toErl(CBlock),
    ?assert(mat:'=='(Expected, Actual)).

small_random_test() ->
    Max = erlBlas:get_max_length(),
    erlBlas:set_max_length(50),
    Sizes = [rand:uniform(10), rand:uniform(10), rand:uniform(10)],
    {timeout, 100, fun() ->
        matrix_test_core(Sizes),
        erlBlas:set_max_length(Max)
    end}.

corner_cases_test_() ->
    Max = erlBlas:get_max_length(),
    erlBlas:set_max_length(50),
    Sizes = [49, 50, 51, 99, 100, 101],
    {timeout, 100, fun() ->
        matrix_test_core(Sizes),
        erlBlas:set_max_length(Max)
    end}.

random_test() ->
    Max = erlBlas:get_max_length(),
    erlBlas:set_max_length(50),
    Sizes = [rand:uniform(800) + 200, rand:uniform(800) + 200, rand:uniform(800) + 200],
    {timeout, 100, fun() ->
        matrix_test_core(Sizes),
        erlBlas:set_max_length(Max)
    end}.

matrix_test_core(Sizes) ->
    lists:map(
        fun(K) ->
            lists:map(
                fun(M) ->
                    lists:map(
                        fun(N) ->
                            matrix_test_basic(K, M, N),
                            matrix_test_transpose(K, M, N),
                            matrix_test_transpose_and_scale(K, M, N)
                        end,
                        Sizes
                    )
                end,
                Sizes
            )
        end,
        Sizes
    ).

matrix_test_basic(K, M, N) ->
    A = generateRandMat(K, M),
    B = generateRandMat(M, N),
    C = generateRandMat(K, N),
    ANumerl = erlBlas:matrix(A),
    BNumerl = erlBlas:matrix(B),
    CNumerl = erlBlas:matrix(C),
    erlBlas:dgemm(false, false, 1, ANumerl, BNumerl, 1, CNumerl),
    ?assert('=='(erlBlas:toErl(CNumerl), eval([A, '*', B, '+', C]))).

matrix_test_transpose(K, M, N) ->
    A = generateRandMat(M, K),
    B = generateRandMat(M, N),
    C = generateRandMat(K, N),
    ANumerl = erlBlas:matrix(A),
    BNumerl = erlBlas:matrix(B),
    CNumerl = erlBlas:matrix(C),
    erlBlas:dgemm(true, false, 1, ANumerl, BNumerl, 1, CNumerl),
    ?assert('=='(erlBlas:toErl(CNumerl), eval([tr(A), '*', B, '+', C]))),
    A2 = generateRandMat(K, M),
    B2 = generateRandMat(N, M),
    C2 = generateRandMat(K, N),
    ANumerl2 = erlBlas:matrix(A2),
    BNumerl2 = erlBlas:matrix(B2),
    CNumerl2 = erlBlas:matrix(C2),
    erlBlas:dgemm(false, true, 1, ANumerl2, BNumerl2, 1, CNumerl2),
    ?assert('=='(erlBlas:toErl(CNumerl2), eval([A2, '*', tr(B2), '+', C2]))),
    A3 = generateRandMat(M, K),
    B3 = generateRandMat(N, M),
    C3 = generateRandMat(K, N),
    ANumerl3 = erlBlas:matrix(A3),
    BNumerl3 = erlBlas:matrix(B3),
    CNumerl3 = erlBlas:matrix(C3),
    erlBlas:dgemm(true, true, 1, ANumerl3, BNumerl3, 1, CNumerl3),
    ?assert('=='(erlBlas:toErl(CNumerl3), eval([tr(A3), '*', tr(B3), '+', C3]))).

matrix_test_transpose_and_scale(K, M, N) ->
    A = generateRandMat(M, K),
    B = generateRandMat(M, N),
    C = generateRandMat(K, N),
    ANumerl = erlBlas:matrix(A),
    BNumerl = erlBlas:matrix(B),
    CNumerl = erlBlas:matrix(C),
    Num1 = rand:uniform(10) / 9,
    erlBlas:dgemm(true, false, Num1, ANumerl, BNumerl, 1, CNumerl),
    ?assert('=='(erlBlas:toErl(CNumerl), eval([Num1, '*', tr(A), '*', B, '+', C]))),
    A2 = generateRandMat(K, M),
    B2 = generateRandMat(N, M),
    C2 = generateRandMat(K, N),
    ANumerl2 = erlBlas:matrix(A2),
    BNumerl2 = erlBlas:matrix(B2),
    CNumerl2 = erlBlas:matrix(C2),
    Num2 = rand:uniform(10) / 9,
    Num3 = rand:uniform(10) / 9,
    erlBlas:dgemm(false, true, Num2, ANumerl2, BNumerl2, Num3, CNumerl2),
    Afois = '*'(Num2, A2),
    Cfois = '*'(Num3, C2),
    AB = '*'(Afois, tr(B2)),
    Result = '+'(AB, Cfois),
    ?assert('=='(erlBlas:toErl(CNumerl2), Result)),
    A3 = generateRandMat(M, K),
    B3 = generateRandMat(N, M),
    C3 = generateRandMat(K, N),
    ANumerl3 = erlBlas:matrix(A3),
    BNumerl3 = erlBlas:matrix(B3),
    CNumerl3 = erlBlas:matrix(C3),
    Num4 = rand:uniform(10) / 9,
    Cfois2 = '*'(Num4, C3),
    AB2 = '*'(tr(A3), tr(B3)),
    Result2 = '+'(AB2, Cfois2),
    erlBlas:dgemm(true, true, 1, ANumerl3, BNumerl3, Num4, CNumerl3),
    ?assert('=='(erlBlas:toErl(CNumerl3), Result2)).

generateRandMat(0, _) ->
    [];
generateRandMat(Dim1, Dim2) ->
    [generateRandVect(Dim2) | generateRandMat(Dim1 - 1, Dim2)].

generateRandVect(0) ->
    [];
generateRandVect(Dim2) ->
    [rand:uniform(5) | generateRandVect(Dim2 - 1)].
