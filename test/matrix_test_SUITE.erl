-module(matrix_test_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("eunit/include/eunit.hrl").

% test done for MAX_LENGTH = 5

check_matrix(M1, M2) ->
    lists:zipwith(
        fun(Row1, Row2) ->
            lists:zipwith(
                fun(Elem1, Elem2) -> ?assert(numerl:equals(Elem1, Elem2)) end,
                Row1,
                Row2
            )
        end,
        M1,
        M2
    ).

base_test() ->
    Max = erlBlas:get_max_length(),
    erlBlas:set_max_length(5),
    M = [[1]],
    Num = numerl:matrix(M),
    Block = erlBlas:matrix(M),
    % also checks the block matrix format is correct
    [[Binary]] = Block,
    ?assert(numerl:equals(Num, Binary)),
    erlBlas:set_max_length(Max).

small_test() ->
    Max = erlBlas:get_max_length(),
    erlBlas:set_max_length(5),
    M = [[1, 2], [3, 4]],
    Num = numerl:matrix(M),
    Block = erlBlas:matrix(M),
    [[Binary]] = Block,
    ?assert(numerl:equals(Num, Binary)),
    erlBlas:set_max_length(Max).

line_test() ->
    Max = erlBlas:get_max_length(),
    erlBlas:set_max_length(5),
    M = [[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]],
    Sub1 = [[1, 2, 3, 4, 5]],
    Sub2 = [[6, 7, 8, 9, 10]],
    Block = erlBlas:matrix(M),
    BlockResult = [[numerl:matrix(Sub1), numerl:matrix(Sub2)]],
    check_matrix(Block, BlockResult),
    erlBlas:set_max_length(Max).

column_test() ->
    Max = erlBlas:get_max_length(),
    erlBlas:set_max_length(5),
    M = [[1], [2], [3], [4], [5], [6], [7], [8], [9], [10]],
    Sub1 = [[1], [2], [3], [4], [5]],
    Sub2 = [[6], [7], [8], [9], [10]],
    Block = erlBlas:matrix(M),
    BlockResult = [[numerl:matrix(Sub1)], [numerl:matrix(Sub2)]],
    check_matrix(Block, BlockResult),
    erlBlas:set_max_length(Max).

max_size_blocks_test() ->
    Max = erlBlas:get_max_length(),
    erlBlas:set_max_length(5),
    M = [
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
    Sub1 =
        [[1, 2, 3, 4, 5], [1, 2, 3, 4, 5], [1, 2, 3, 4, 5], [1, 2, 3, 4, 5], [1, 2, 3, 4, 5]],
    Sub2 =
        [
            [6, 7, 8, 9, 10],
            [6, 7, 8, 9, 10],
            [6, 7, 8, 9, 10],
            [6, 7, 8, 9, 10],
            [6, 7, 8, 9, 10]
        ],
    Block = erlBlas:matrix(M),
    BlockResult =
        [[numerl:matrix(Sub1), numerl:matrix(Sub2)], [numerl:matrix(Sub1), numerl:matrix(Sub2)]],
    check_matrix(Block, BlockResult),
    erlBlas:set_max_length(Max).

rest_test() ->
    Max = erlBlas:get_max_length(),
    erlBlas:set_max_length(5),
    M = [
        [1, 2, 3, 4, 5, 6, 7],
        [1, 2, 3, 4, 5, 6, 7],
        [1, 2, 3, 4, 5, 6, 7],
        [1, 2, 3, 4, 5, 6, 7],
        [1, 2, 3, 4, 5, 6, 7],
        [1, 2, 3, 4, 5, 6, 7],
        [1, 2, 3, 4, 5, 6, 7]
    ],
    Sub1 = [[1, 2], [1, 2]],
    Sub2 = [[3, 4, 5, 6, 7], [3, 4, 5, 6, 7]],
    Sub3 = [[1, 2], [1, 2], [1, 2], [1, 2], [1, 2]],
    Sub4 =
        [[3, 4, 5, 6, 7], [3, 4, 5, 6, 7], [3, 4, 5, 6, 7], [3, 4, 5, 6, 7], [3, 4, 5, 6, 7]],
    Block = erlBlas:matrix(M),
    BlockResult =
        [[numerl:matrix(Sub1), numerl:matrix(Sub2)], [numerl:matrix(Sub3), numerl:matrix(Sub4)]],
    check_matrix(Block, BlockResult),
    erlBlas:set_max_length(Max).
