-module(dgemm_test_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(mat, ['*'/2, '*´'/2, tr/1, '+'/2, '-'/2, '=='/2, eval/1]).
-import(erlBlas, [dgemm/7]).

-export([square_matrix_test_basic/0, square_matrix_test_transpose/0,
         square_matrix_test_transpose_and_scale/0, rectangular_matrix_test_basic/0,
         rectangular_matrix_test_transpose/0, rectangular_matrix_test_transpose_and_scale/0]).

square_matrix_test_() ->
    {timeout, 30, fun() -> square_matrix_test_core() end}.

square_matrix_test_core() ->
    run_x_times(square_matrix_test_basic, 2),
    run_x_times(square_matrix_test_transpose, 2),
    run_x_times(square_matrix_test_transpose_and_scale, 2).

square_matrix_test_basic() ->
    A = generateRandMat(50, 50),
    B = generateRandMat(50, 50),
    C = generateRandMat(50, 50),
    ANumerl = erlBlas:matrix(A),
    BNumerl = erlBlas:matrix(B),
    CNumerl = erlBlas:matrix(C),
    erlBlas:dgemm(false, false, 1, ANumerl, BNumerl, 1, CNumerl),
    ?assert('=='(erlBlas:toErl(CNumerl), eval([A, '*', B, '+', C]))).

square_matrix_test_transpose() ->
    A = generateRandMat(50, 50),
    B = generateRandMat(50, 50),
    C = generateRandMat(50, 50),
    ANumerl = erlBlas:matrix(A),
    BNumerl = erlBlas:matrix(B),
    CNumerl = erlBlas:matrix(C),
    erlBlas:dgemm(true, false, 1, ANumerl, BNumerl, 1, CNumerl),
    ?assert('=='(erlBlas:toErl(CNumerl), eval([tr(A), '*', B, '+', C]))),
    A2 = generateRandMat(50, 50),
    B2 = generateRandMat(50, 50),
    C2 = generateRandMat(50, 50),
    ANumerl2 = erlBlas:matrix(A2),
    BNumerl2 = erlBlas:matrix(B2),
    CNumerl2 = erlBlas:matrix(C2),
    erlBlas:dgemm(false, true, 1, ANumerl2, BNumerl2, 1, CNumerl2),
    ?assert('=='(erlBlas:toErl(CNumerl2), eval([A2, '*', tr(B2), '+', C2]))),
    A3 = generateRandMat(50, 50),
    B3 = generateRandMat(50, 50),
    C3 = generateRandMat(50, 50),
    ANumerl3 = erlBlas:matrix(A3),
    BNumerl3 = erlBlas:matrix(B3),
    CNumerl3 = erlBlas:matrix(C3),
    erlBlas:dgemm(true, true, 1, ANumerl3, BNumerl3, 1, CNumerl3),
    ?assert('=='(erlBlas:toErl(CNumerl3), eval([tr(A3), '*', tr(B3), '+', C3]))).

square_matrix_test_transpose_and_scale() ->
    A = generateRandMat(50, 50),
    B = generateRandMat(50, 50),
    C = generateRandMat(50, 50),
    ANumerl = erlBlas:matrix(A),
    BNumerl = erlBlas:matrix(B),
    CNumerl = erlBlas:matrix(C),
    Num1 = rand:uniform(10) / 9,
    erlBlas:dgemm(true, false, Num1, ANumerl, BNumerl, 1, CNumerl),
    ?assert('=='(erlBlas:toErl(CNumerl), eval([Num1, '*', tr(A), '*', B, '+', C]))),
    A2 = generateRandMat(50, 50),
    B2 = generateRandMat(50, 50),
    C2 = generateRandMat(50, 50),
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
    A3 = generateRandMat(50, 50),
    B3 = generateRandMat(50, 50),
    C3 = generateRandMat(50, 50),
    ANumerl3 = erlBlas:matrix(A3),
    BNumerl3 = erlBlas:matrix(B3),
    CNumerl3 = erlBlas:matrix(C3),
    Num4 = rand:uniform(10) / 9,
    Cfois2 = '*'(Num4, C3),
    AB2 = '*'(tr(A3), tr(B3)),
    Result2 = '+'(AB2, Cfois2),
    erlBlas:dgemm(true, true, 1, ANumerl3, BNumerl3, Num4, CNumerl3),
    ?assert('=='(erlBlas:toErl(CNumerl3), Result2)).

rectangular_matrix_test_() ->
    {timeout, 30, fun() -> rectangular_matrix_test_core() end}.

rectangular_matrix_test_core() ->
    run_x_times(rectangular_matrix_test_basic, 5),
    run_x_times(rectangular_matrix_test_transpose, 5),
    run_x_times(rectangular_matrix_test_transpose_and_scale, 5).

rectangular_matrix_test_basic() ->
    K = rand:uniform(50),
    M = rand:uniform(50),
    N = rand:uniform(50),
    A = generateRandMat(K, M),
    B = generateRandMat(M, N),
    C = generateRandMat(K, N),
    ANumerl = erlBlas:matrix(A),
    BNumerl = erlBlas:matrix(B),
    CNumerl = erlBlas:matrix(C),
    erlBlas:dgemm(false, false, 1, ANumerl, BNumerl, 1, CNumerl),
    ?assert('=='(erlBlas:toErl(CNumerl), eval([A, '*', B, '+', C]))).

rectangular_matrix_test_transpose() ->
    K = rand:uniform(50),
    M = rand:uniform(50),
    N = rand:uniform(50),
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

rectangular_matrix_test_transpose_and_scale() ->
    K = rand:uniform(50),
    M = rand:uniform(50),
    N = rand:uniform(50),
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

run_x_times(_, 0) ->
    ok;
run_x_times(Fun, X) ->
    ?assert(X >= 0),
    F = fun dgemm_test_SUITE:Fun/0,
    F(),
    run_x_times(Fun, X - 1).

generateRandMat(0, _) ->
    [];
generateRandMat(Dim1, Dim2) ->
    [generateRandVect(Dim2) | generateRandMat(Dim1 - 1, Dim2)].

generateRandVect(0) ->
    [];
generateRandVect(Dim2) ->
    [rand:uniform(5) | generateRandVect(Dim2 - 1)].
