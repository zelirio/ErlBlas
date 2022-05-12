-module(add_conc_test_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("eunit/include/eunit.hrl").

base_test() ->
    A = [[1]],
    B = [[1]],
    C = [[2]],
    ABlock = block_mat:matrix(A),
    BBlock = block_mat:matrix(B),
    CBlock = block_mat:matrix(C),
    Res = block_mat:add_conc(ABlock, BBlock),
    ?assert(block_mat:equals(Res, CBlock)).

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

    ABlock = block_mat:matrix(A),
    BBlock = block_mat:matrix(B),
    CBlock = block_mat:matrix(C),
    Res = block_mat:add_conc(ABlock, BBlock),
    ?assert(block_mat:equals(Res, CBlock)).

float_test() ->
    A = [[1.2, 2.5, 3.6, 4.7, 5.69, 42.69], [6.24, 7.77, 8.42, 9.58, 10.013, 69.42]],

    B = [[1.2, 2.5, 3.6, 4.7, 5.69, 42.69], [6.24, 7.77, 8.42, 9.58, 10.013, 69.42]],

    ABlock = block_mat:matrix(A),
    BBlock = block_mat:matrix(B),
    Res = block_mat:add_conc(ABlock, BBlock),
    ANum = numerl:matrix(A),
    BNum = numerl:matrix(B),
    Conf = numerl:add(ANum, BNum),
    Expected = numerl:mtfl(Conf),
    Actual = block_mat:toErl(Res),
    ?assert(mat:'=='(Expected, Actual)).

random_square_test() ->
    N = 13,
    A = utils:generateRandMat(N, N),
    B = utils:generateRandMat(N, N),
    ABlock = block_mat:matrix(A),
    BBlock = block_mat:matrix(B),
    Res = block_mat:add_conc(ABlock, BBlock),
    ANum = numerl:matrix(A),
    BNum = numerl:matrix(B),
    Conf = numerl:add(ANum, BNum),
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
    Res = block_mat:add_conc(ABlock, BBlock),
    ANum = numerl:matrix(A),
    BNum = numerl:matrix(B),
    Conf = numerl:add(ANum, BNum),
    Expected = numerl:mtfl(Conf),
    Actual = block_mat:toErl(Res),
    ?assert(mat:'=='(Expected, Actual)).

performance_test_() ->
    {timeout,
     100,
     fun() ->
        performance(),
        performance_conc()
     end}.

performance_conc() ->
    timer:sleep(100),
    %add_conc_exec_time(10,100),
    Sizes = [500],
    Results =
        lists:map(fun(Size) ->
                     Times = add_conc_exec_time(10, Size),
                     stats(Times)
                  end,
                  Sizes),
    erlang:display({conc, Results}).

add_conc_exec_time(N, Size) ->
    M1 = utils:generateRandMat(Size, Size),
    M2 = utils:generateRandMat(Size, Size),
    Mat1 = block_mat:matrix(M1),
    Mat2 = block_mat:matrix(M2),
    {Time, _} = timer:tc(block_mat, add_conc2, [Mat1, Mat2]),
    if N == 1 ->
           [Time];
       true ->
           [Time | add_conc_exec_time(N - 1, Size)]
    end.

performance() ->
    timer:sleep(100),
    %add_exec_time(10,100),
    Sizes = [500],
    Results =
        lists:map(fun(Size) ->
                     Times = add_exec_time(10, Size),
                     stats(Times)
                  end,
                  Sizes),
    erlang:display({seq, Results}).

add_exec_time(N, Size) ->
    M1 = utils:generateRandMat(Size, Size),
    M2 = utils:generateRandMat(Size, Size),
    Mat1 = block_mat:matrix(M1),
    Mat2 = block_mat:matrix(M2),
    {Time, _} = timer:tc(block_mat, add2, [Mat1, Mat2]),
    if N == 1 ->
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

% multicore_test() ->
%     M1 = utils:generateRandMat(1000, 100),
%     M2 = utils:generateRandMat(1000, 100),
%     Mat1 = numerl:matrix(M1),
%     Mat2 = numerl:matrix(M2),
%     TwoPids =
%         [spawn(fun() ->
%                   erlang:display({first_instr, self()}),
%                   numerl:add(Mat1, Mat2)
%                end),
%          spawn(fun() ->
%                   erlang:display({first_instr, self()}),
%                   numerl:add(Mat1, Mat2)
%                end)],
%     erlang:display(TwoPids).
