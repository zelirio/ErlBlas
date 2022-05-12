-module(matrix_test_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("eunit/include/eunit.hrl").

% test done for MAX_LENGTH = 5

check_matrix(M1, M2) ->
    lists:zipwith(fun(Row1, Row2) ->
                     lists:zipwith(fun(Elem1, Elem2) -> ?assert(numerl:equals(Elem1, Elem2)) end,
                                   Row1,
                                   Row2)
                  end,
                  M1,
                  M2).

base_test() ->
    block_mat:set_max_length(5),
    M = [[1]],
    Num = numerl:matrix(M),
    Block = block_mat:matrix(M),
    [[Binary]] = Block, % also checks the block matrix format is correct
    ?assert(numerl:equals(Num, Binary)).

small_test() ->
    M = [[1, 2], [3, 4]],
    Num = numerl:matrix(M),
    Block = block_mat:matrix(M),
    [[Binary]] = Block,
    ?assert(numerl:equals(Num, Binary)).

line_test() ->
    M = [[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]],
    Sub1 = [[1, 2, 3, 4, 5]],
    Sub2 = [[6, 7, 8, 9, 10]],
    Block = block_mat:matrix(M),
    BlockResult = [[numerl:matrix(Sub1), numerl:matrix(Sub2)]],
    check_matrix(Block, BlockResult).

column_test() ->
    M = [[1], [2], [3], [4], [5], [6], [7], [8], [9], [10]],
    Sub1 = [[1], [2], [3], [4], [5]],
    Sub2 = [[6], [7], [8], [9], [10]],
    Block = block_mat:matrix(M),
    BlockResult = [[numerl:matrix(Sub1)], [numerl:matrix(Sub2)]],
    check_matrix(Block, BlockResult).

max_size_blocks_test() ->
    M = [[1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
         [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
         [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
         [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
         [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
         [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
         [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
         [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
         [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
         [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]],
    Sub1 =
        [[1, 2, 3, 4, 5], [1, 2, 3, 4, 5], [1, 2, 3, 4, 5], [1, 2, 3, 4, 5], [1, 2, 3, 4, 5]],
    Sub2 =
        [[6, 7, 8, 9, 10],
         [6, 7, 8, 9, 10],
         [6, 7, 8, 9, 10],
         [6, 7, 8, 9, 10],
         [6, 7, 8, 9, 10]],
    Block = block_mat:matrix(M),
    BlockResult =
        [[numerl:matrix(Sub1), numerl:matrix(Sub2)], [numerl:matrix(Sub1), numerl:matrix(Sub2)]],
    check_matrix(Block, BlockResult).

rest_test() ->
    M = [[1, 2, 3, 4, 5, 6, 7],
         [1, 2, 3, 4, 5, 6, 7],
         [1, 2, 3, 4, 5, 6, 7],
         [1, 2, 3, 4, 5, 6, 7],
         [1, 2, 3, 4, 5, 6, 7],
         [1, 2, 3, 4, 5, 6, 7],
         [1, 2, 3, 4, 5, 6, 7]],
    Sub1 = [[1, 2], [1, 2]],
    Sub2 = [[3, 4, 5, 6, 7], [3, 4, 5, 6, 7]],
    Sub3 = [[1, 2], [1, 2], [1, 2], [1, 2], [1, 2]],
    Sub4 =
        [[3, 4, 5, 6, 7], [3, 4, 5, 6, 7], [3, 4, 5, 6, 7], [3, 4, 5, 6, 7], [3, 4, 5, 6, 7]],
    Block = block_mat:matrix(M),
    BlockResult =
        [[numerl:matrix(Sub1), numerl:matrix(Sub2)], [numerl:matrix(Sub3), numerl:matrix(Sub4)]],
    check_matrix(Block, BlockResult),
    block_mat:set_max_length(58).
