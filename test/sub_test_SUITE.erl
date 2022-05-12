-module(sub_test_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("eunit/include/eunit.hrl").

base_test() ->
    A = [[1]],
    B = [[1]],
    C = [[0]],
    ABlock = block_mat:matrix(A),
    BBlock = block_mat:matrix(B),
    CBlock = block_mat:matrix(C),
    Res = block_mat:sub_conc(ABlock, BBlock),
    ?assert(block_mat:equals(Res, CBlock)).

float_test() ->
    A = [[1.2, 2.5, 3.6, 4.7, 5.69, 42.69], [6.24, 7.77, 8.42, 9.58, 10.013, 69.42]],

    B = [[1.2, 2.5, 3.6, 4.7, 5.69, 42.69], [6.24, 7.77, 8.42, 9.58, 10.013, 69.42]],

    ABlock = block_mat:matrix(A),
    BBlock = block_mat:matrix(B),
    Res = block_mat:sub_conc(ABlock, BBlock),
    ANum = numerl:matrix(A),
    BNum = numerl:matrix(B),
    Conf = numerl:sub(ANum, BNum),
    Expected = numerl:mtfl(Conf),
    Actual = block_mat:toErl(Res),
    ?assert(mat:'=='(Expected, Actual)).

random_square_test() ->
    N = 13,
    A = utils:generateRandMat(N, N),
    B = utils:generateRandMat(N, N),
    ABlock = block_mat:matrix(A),
    BBlock = block_mat:matrix(B),
    Res = block_mat:sub_conc(ABlock, BBlock),
    ANum = numerl:matrix(A),
    BNum = numerl:matrix(B),
    Conf = numerl:sub(ANum, BNum),
    Expected = numerl:mtfl(Conf),
    Actual = block_mat:toErl(Res),
    ?assert(mat:'=='(Expected, Actual)).

random_rectangle_test() ->
    N = 13,
    M = 7,
    A = utils:generateRandMat(N, M),
    B = utils:generateRandMat(N, M),
    ABlock = block_mat:matrix(A),
    BBlock = block_mat:matrix(B),
    Res = block_mat:sub_conc(ABlock, BBlock),
    ANum = numerl:matrix(A),
    BNum = numerl:matrix(B),
    Conf = numerl:sub(ANum, BNum),
    Expected = numerl:mtfl(Conf),
    Actual = block_mat:toErl(Res),
    ?assert(mat:'=='(Expected, Actual)).
