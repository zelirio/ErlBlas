-module(inv_test_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("eunit/include/eunit.hrl").

base_test() ->
    M = [[1]],
    Num = numerl:matrix(M),
    Block = erlBlas:matrix(M),
    Res = numerl:inv(Num),
    Inv = erlBlas:inv(Block),
    ErlRes = numerl:mtfl(Res),
    ErlInv = erlBlas:toErl(Inv),
    ?assert(mat:'=='(ErlInv, ErlRes)).

max_size_blocks_test() ->
    M = [[1, 27, 31, 45, 54, 6, 7, 8, 9, 10],
         [10, 25, 38, 4, 5, 6, 7, 8, 9, 11],
         [1, 20, 34, 4, 58, 6, 72, 78, 98, 12],
         [11, 22, 32, 4, 5, 60, 7, 8, 9, 13],
         [1, 2, 37, 4, 51, 61, 71, 8, 97, 14],
         [10, 22, 30, 4, 5, 68, 7, 8, 89, 15],
         [11, 2, 34, 4, 5, 67, 7, 80, 9, 16],
         [1, 25, 53, 47, 5, 6, 79, 8, 97, 17],
         [18, 27, 3, 48, 5, 6, 74, 84, 9, 18],
         [1, 20, 36, 4, 5, 65, 71, 87, 9, 19]],
    Num = numerl:matrix(M),
    NumResult = numerl:inv(Num),
    ErlNum = numerl:mtfl(NumResult),
    Block = erlBlas:matrix(M),
    BlockResult = erlBlas:inv(Block),
    ErlBlock = erlBlas:toErl(BlockResult),
    ?assert(mat:'=='(ErlBlock, ErlNum)).

random_test() ->
    N = 13,
    M = utils:generateRandMat(N, N),
    Num = numerl:matrix(M),
    NumResult = numerl:inv(Num),
    ErlNum = numerl:mtfl(NumResult),
    Block = erlBlas:matrix(M),
    BlockResult = erlBlas:inv(Block),
    ErlBlock = erlBlas:toErl(BlockResult),
    ?assert(mat:'=='(ErlBlock, ErlNum)).
