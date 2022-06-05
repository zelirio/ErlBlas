-module(zeros_test_SUITE).

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
    Num = numerl:zeros(1, 1),
    Block = erlBlas:zeros(1, 1),
    % also checks the block matrix format is correct
    [[Binary]] = Block,
    ?assert(numerl:equals(Num, Binary)),
    erlBlas:set_max_length(Max).

small_test() ->
    Max = erlBlas:get_max_length(),
    erlBlas:set_max_length(5),
    Num = numerl:zeros(2, 2),
    Block = erlBlas:zeros(2, 2),
    [[Binary]] = Block,
    ?assert(numerl:equals(Num, Binary)),
    erlBlas:set_max_length(Max).

line_test() ->
    Max = erlBlas:get_max_length(),
    erlBlas:set_max_length(5),
    Block = erlBlas:zeros(1, 10),
    BlockResult = [[numerl:zeros(1, 5), numerl:zeros(1, 5)]],
    check_matrix(Block, BlockResult),
    erlBlas:set_max_length(Max).

column_test() ->
    Max = erlBlas:get_max_length(),
    erlBlas:set_max_length(5),
    Block = erlBlas:zeros(10, 1),
    BlockResult = [[numerl:zeros(5, 1)], [numerl:zeros(5, 1)]],
    check_matrix(Block, BlockResult),
    erlBlas:set_max_length(Max).

max_size_blocks_test() ->
    Max = erlBlas:get_max_length(),
    erlBlas:set_max_length(5),
    Block = erlBlas:zeros(10, 10),
    BlockResult =
        [[numerl:zeros(5, 5), numerl:zeros(5, 5)], [numerl:zeros(5, 5), numerl:zeros(5, 5)]],
    check_matrix(Block, BlockResult),
    erlBlas:set_max_length(Max).

rest_test() ->
    Max = erlBlas:get_max_length(),
    erlBlas:set_max_length(5),
    Block = erlBlas:zeros(7, 7),
    BlockResult =
        [[numerl:zeros(2, 2), numerl:zeros(2, 5)], [numerl:zeros(5, 2), numerl:zeros(5, 5)]],
    check_matrix(Block, BlockResult),
    erlBlas:set_max_length(Max).
