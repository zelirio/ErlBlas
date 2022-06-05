-module(eye_test_SUITE).

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
    Num = numerl:eye(1),
    Block = erlBlas:eye(1),
    % also checks the block matrix format is correct
    [[Binary]] = Block,
    ?assert(numerl:equals(Num, Binary)),
    erlBlas:set_max_length(Max).

small_test() ->
    Max = erlBlas:get_max_length(),
    erlBlas:set_max_length(5),
    Num = numerl:eye(2),
    Block = erlBlas:eye(2),
    [[Binary]] = Block,
    ?assert(numerl:equals(Num, Binary)),
    erlBlas:set_max_length(Max).

max_size_blocks_test() ->
    Max = erlBlas:get_max_length(),
    erlBlas:set_max_length(5),
    Block = erlBlas:eye(10),
    BlockResult =
        [[numerl:eye(5), numerl:zeros(5, 5)], [numerl:zeros(5, 5), numerl:eye(5)]],
    check_matrix(Block, BlockResult),
    erlBlas:set_max_length(Max).

rest_test() ->
    Max = erlBlas:get_max_length(),
    erlBlas:set_max_length(5),
    Block = erlBlas:eye(7),
    BlockResult =
        [[numerl:eye(2), numerl:zeros(2, 5)], [numerl:zeros(5, 2), numerl:eye(5)]],
    check_matrix(Block, BlockResult),
    erlBlas:set_max_length(Max).
