-module(add_test_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("eunit/include/eunit.hrl").

base_test() ->
    A = [[1]],
    B = [[1]],
    C = [[2]],
    ABlock = erlBlas:matrix(A),
    BBlock = erlBlas:matrix(B),
    CBlock = erlBlas:matrix(C),
    Res = erlBlas:add(ABlock, BBlock),
    ?assert(erlBlas:equals(Res, CBlock)).

max_size_blocks_test() ->
    A = [[1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
         [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
         [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
         [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
         [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
         [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
         [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
         [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
         [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
         [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]],

    B = [[2, 4, 6, 8, 10, 12, 14, 16, 18, 20],
         [2, 4, 6, 8, 10, 12, 14, 16, 18, 20],
         [2, 4, 6, 8, 10, 12, 14, 16, 18, 20],
         [2, 4, 6, 8, 10, 12, 14, 16, 18, 20],
         [2, 4, 6, 8, 10, 12, 14, 16, 18, 20],
         [2, 4, 6, 8, 10, 12, 14, 16, 18, 20],
         [2, 4, 6, 8, 10, 12, 14, 16, 18, 20],
         [2, 4, 6, 8, 10, 12, 14, 16, 18, 20],
         [2, 4, 6, 8, 10, 12, 14, 16, 18, 20],
         [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]],

    C = [[3, 6, 9, 12, 15, 18, 21, 24, 27, 30],
         [3, 6, 9, 12, 15, 18, 21, 24, 27, 30],
         [3, 6, 9, 12, 15, 18, 21, 24, 27, 30],
         [3, 6, 9, 12, 15, 18, 21, 24, 27, 30],
         [3, 6, 9, 12, 15, 18, 21, 24, 27, 30],
         [3, 6, 9, 12, 15, 18, 21, 24, 27, 30],
         [3, 6, 9, 12, 15, 18, 21, 24, 27, 30],
         [3, 6, 9, 12, 15, 18, 21, 24, 27, 30],
         [3, 6, 9, 12, 15, 18, 21, 24, 27, 30],
         [3, 6, 9, 12, 15, 18, 21, 24, 27, 30]],

    ABlock = erlBlas:matrix(A),
    BBlock = erlBlas:matrix(B),
    CBlock = erlBlas:matrix(C),
    Res = erlBlas:add(ABlock, BBlock),
    ?assert(erlBlas:equals(Res, CBlock)).

float_test() ->
    A = [[1.2, 2.5, 3.6, 4.7, 5.69, 42.69], [6.24, 7.77, 8.42, 9.58, 10.013, 69.42]],

    B = [[1.2, 2.5, 3.6, 4.7, 5.69, 42.69], [6.24, 7.77, 8.42, 9.58, 10.013, 69.42]],

    ABlock = erlBlas:matrix(A),
    BBlock = erlBlas:matrix(B),
    Res = erlBlas:add(ABlock, BBlock),
    ANum = numerl:matrix(A),
    BNum = numerl:matrix(B),
    Conf = numerl:add(ANum, BNum),
    Expected = numerl:mtfl(Conf),
    Actual = erlBlas:toErl(Res),
    ?assert(mat:'=='(Expected, Actual)).

random_square_test() ->
    N = 13,
    A = utils:generateRandMat(N, N),
    B = utils:generateRandMat(N, N),
    ABlock = erlBlas:matrix(A),
    BBlock = erlBlas:matrix(B),
    Res = erlBlas:add(ABlock, BBlock),
    ANum = numerl:matrix(A),
    BNum = numerl:matrix(B),
    Conf = numerl:add(ANum, BNum),
    Expected = numerl:mtfl(Conf),
    Actual = erlBlas:toErl(Res),
    ?assert(mat:'=='(Expected, Actual)).

random_rectangle_test() ->
    N = 13,
    M = 7,
    A = utils:generateRandMat(N, M),
    B = utils:generateRandMat(N, M),
    ABlock = erlBlas:matrix(A),
    BBlock = erlBlas:matrix(B),
    Res = erlBlas:add(ABlock, BBlock),
    ANum = numerl:matrix(A),
    BNum = numerl:matrix(B),
    Conf = numerl:add(ANum, BNum),
    Expected = numerl:mtfl(Conf),
    Actual = erlBlas:toErl(Res),
    ?assert(mat:'=='(Expected, Actual)).
