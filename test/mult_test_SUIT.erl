-module(mult_test_SUIT).
-include_lib("stdlib/include/assert.hrl").
%-include_lib("eunit/include/eunit.hrl").

base_test() ->
    A = [[1]],
    B = [[1]],
    C = [[1]],
    ABlock = block_mat:matrix(A),
    BBlock = block_mat:matrix(B),
    CBlock = block_mat:matrix(C),
    Res = block_mat:mult(ABlock, BBlock),
    ?assert(block_mat:equals(Res, CBlock)).

max_size_blocks_test() ->
    A =[[1,2,3,4,5,6,7,8,9,10],
        [1,2,3,4,5,6,7,8,9,10],
        [1,2,3,4,5,6,7,8,9,10],
        [1,2,3,4,5,6,7,8,9,10],
        [1,2,3,4,5,6,7,8,9,10],
        [1,2,3,4,5,6,7,8,9,10],
        [1,2,3,4,5,6,7,8,9,10],
        [1,2,3,4,5,6,7,8,9,10],
        [1,2,3,4,5,6,7,8,9,10],
        [1,2,3,4,5,6,7,8,9,10]],

    B = [[2,4,6,8,10,12,14,16,18,20],
         [2,4,6,8,10,12,14,16,18,20],
         [2,4,6,8,10,12,14,16,18,20],
         [2,4,6,8,10,12,14,16,18,20],
         [2,4,6,8,10,12,14,16,18,20],
         [2,4,6,8,10,12,14,16,18,20],
         [2,4,6,8,10,12,14,16,18,20],
         [2,4,6,8,10,12,14,16,18,20],
         [2,4,6,8,10,12,14,16,18,20],
         [2,4,6,8,10,12,14,16,18,20]],

    C = [[110,220,330,440,550,660,770,880,990,1100],
         [110,220,330,440,550,660,770,880,990,1100],
         [110,220,330,440,550,660,770,880,990,1100],
         [110,220,330,440,550,660,770,880,990,1100],
         [110,220,330,440,550,660,770,880,990,1100],
         [110,220,330,440,550,660,770,880,990,1100],
         [110,220,330,440,550,660,770,880,990,1100],
         [110,220,330,440,550,660,770,880,990,1100],
         [110,220,330,440,550,660,770,880,990,1100],
         [110,220,330,440,550,660,770,880,990,1100]],

    ABlock = block_mat:matrix(A),
    BBlock = block_mat:matrix(B),
    CBlock = block_mat:matrix(C),
    Res = block_mat:mult(ABlock, BBlock),
    ?assert(block_mat:equals(Res, CBlock)).

float_test() ->
    A = [[-1.2,2.5,3.6,4.7,5.69, 42.69],
         [6.24,7.77,8.42,-9.58,10.013, 69.42]],

    B = [[1.2,2.5],[3.6,4.7],[-5.69, 42.69],
         [6.24,-7.77],[8.42,9.58],[-10.013, 69.42]],

    ABlock = block_mat:matrix(A),
    BBlock = block_mat:matrix(B),
    Res = block_mat:mult(ABlock, BBlock),
    ANum = numerl:matrix(A),
    BNum = numerl:matrix(B),
    Conf = numerl:dot(ANum, BNum),
    Expected = numerl:mtfl(Conf),
    Actual = block_mat:toErl(Res),
    ?assert(mat:'=='(Expected, Actual)).

random_square_test() ->
    N = 13,
    A = utils:generateRandMat(N, N),
    B = utils:generateRandMat(N, N),
    ABlock = block_mat:matrix(A),
    BBlock = block_mat:matrix(B),
    Res = block_mat:mult(ABlock, BBlock),
    ANum = numerl:matrix(A),
    BNum = numerl:matrix(B),
    Conf = numerl:dot(ANum, BNum),
    Expected = numerl:mtfl(Conf),
    Actual = block_mat:toErl(Res),
    ?assert(mat:'=='(Expected, Actual)).

random_rectangle_test() ->
    N = 13,
    M = 7,
    A = utils:generateRandMat(N, M),
    B = utils:generateRandMat(M, N),
    ABlock = block_mat:matrix(A),
    BBlock = block_mat:matrix(B),
    Res = block_mat:mult(ABlock, BBlock),
    ANum = numerl:matrix(A),
    BNum = numerl:matrix(B),
    Conf = numerl:dot(ANum, BNum),
    Expected = numerl:mtfl(Conf),
    Actual = block_mat:toErl(Res),
    ?assert(mat:'=='(Expected, Actual)).


dgemm_test() ->
    N = 3,
    A = [[1,2,3],[4,5,6],[7,8,9]],
    B = [[1,2,3],[4,5,6],[7,8,9]],
    ABlock = block_mat:matrix(A),
    BBlock = block_mat:matrix(B),
    C = [[1,2,3],[4,5,6],[7,8,9]],
    CBlock = block_mat:matrix(C),
    Gnagna = block_mat:transpose(BBlock),
    Gnagna2 = block_mat:transpose(ABlock),
    Res = block_mat:dgemm(true, true, 1.0, Gnagna2, Gnagna, 1.0, CBlock),
    timer:sleep(100),
    ANum = numerl:matrix(A),
    BNum = numerl:matrix(B),
    CNum = numerl:matrix(C),
    Conf = numerl:dot(ANum, BNum),
    Expected = numerl:mtfl(numerl:add(Conf,CNum)),
    Actual = block_mat:toErl(CBlock),
    ?assert(mat:'=='(Expected, Actual)).
