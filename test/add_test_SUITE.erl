-module(add_test_SUITE).
-import(erlBlas, []).

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
        [3, 6, 9, 12, 15, 18, 21, 24, 27, 30],
        [3, 6, 9, 12, 15, 18, 21, 24, 27, 30],
        [3, 6, 9, 12, 15, 18, 21, 24, 27, 30],
        [3, 6, 9, 12, 15, 18, 21, 24, 27, 30],
        [3, 6, 9, 12, 15, 18, 21, 24, 27, 30],
        [3, 6, 9, 12, 15, 18, 21, 24, 27, 30],
        [3, 6, 9, 12, 15, 18, 21, 24, 27, 30],
        [3, 6, 9, 12, 15, 18, 21, 24, 27, 30],
        [3, 6, 9, 12, 15, 18, 21, 24, 27, 30],
        [3, 6, 9, 12, 15, 18, 21, 24, 27, 30]
    ],

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
    Sizes = [rand:uniform(800) + 500, rand:uniform(800) + 500, rand:uniform(800) + 500],
    {timeout, 100, fun() ->
        matrix_test_core(Sizes),
        erlBlas:set_max_length(Max)
    end}.

matrix_test_core(Sizes) ->
    lists:map(
        fun(M) ->
            lists:map(
                fun(N) ->
                    random_test_core(M, N)
                end,
                Sizes
            )
        end,
        Sizes
    ).

random_test_core(M, N) ->
    A = utils:generateRandMat(M, N),
    B = utils:generateRandMat(M, N),
    ABlock = erlBlas:matrix(A),
    BBlock = erlBlas:matrix(B),
    Res = erlBlas:add(ABlock, BBlock),
    ANum = numerl:matrix(A),
    BNum = numerl:matrix(B),
    Conf = numerl:add(ANum, BNum),
    Expected = numerl:mtfl(Conf),
    Actual = erlBlas:toErl(Res),
    ?assert(mat:'=='(Expected, Actual)).

performance_test_() ->
    Max = erlBlas:get_max_length(),
    {timeout, 1000, fun() ->
        MaxLengths = [5, 10, 50, 100, 200, 500],
        lists:map(
            fun(MaxLength) ->
                erlBlas:set_max_length(MaxLength),
                erlang:display({max_length, erlBlas:get_max_length()}),
                performance(),
                performance_conc(),
                erlBlas:set_max_length(Max)
            end,
            MaxLengths
        )
    end}.

performance_conc() ->
    timer:sleep(100),
    %add_exec_time(10,100),

    %,1000,2000],
    Sizes = [10, 50, 100, 200, 350, 500],
    Results =
        lists:map(
            fun(Size) ->
                Times = add_conc_exec_time(40, Size),
                stats(Times)
            end,
            Sizes
        ),
    erlang:display({conc, Results}).

add_conc_exec_time(N, Size) ->
    M1 = utils:generateRandMat(Size, Size),
    M2 = utils:generateRandMat(Size, Size),
    Mat1 = erlBlas:matrix(M1),
    Mat2 = erlBlas:matrix(M2),
    {Time, _} = timer:tc(erlBlas, add, [Mat1, Mat2]),
    if
        N == 1 ->
            [Time];
        true ->
            [Time | add_conc_exec_time(N - 1, Size)]
    end.

performance() ->
    timer:sleep(100),
    %add_exec_time(10,100),

    %,1000,2000],
    Sizes = [10, 50, 100, 200, 350, 500],
    Results =
        lists:map(
            fun(Size) ->
                Times = add_exec_time(40, Size),
                stats(Times)
            end,
            Sizes
        ),
    erlang:display({seq, Results}).

add_exec_time(N, Size) ->
    M1 = utils:generateRandMat(Size, Size),
    M2 = utils:generateRandMat(Size, Size),
    Mat1 = erlBlas:matrix(M1),
    Mat2 = erlBlas:matrix(M2),
    {Time, _} = timer:tc(sequential, add, [Mat1, Mat2]),
    if
        N == 1 ->
            [Time];
        true ->
            [Time | add_exec_time(N - 1, Size)]
    end.

stats(List) ->
    case List of
        [X] ->
            {X, 0};
        _ ->
            {mean(List), var(List, mean(List))}
    end.

mean(List) ->
    %erlang:display(List),
    lineSum(List) / length(List).

var(List, Mean) ->
    lineSum(lists:map(fun(Elem) -> (Elem - Mean) * (Elem - Mean) end, List)) / length(List).

lineSum([H | T]) ->
    lineSum(T, H).

lineSum(List, Acc) ->
    case List of
        [H | T] ->
            lineSum(T, Acc + H);
        [] ->
            Acc
    end.
