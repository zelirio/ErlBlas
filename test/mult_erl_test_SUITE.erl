-module(mult_erl_test_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("eunit/include/eunit.hrl").

performance_test_() ->
    {timeout,
    1000,
     fun() ->
        performance_erl(),
        performance_C()
    end}.

performance_C() ->
    timer:sleep(100),
    Sizes = [10,50,100,200],%,1000,2000],
    Results =
        lists:map(fun(Size) ->
                    Times = mult_C_exec_time(100, Size),
                    stats(Times)
                end,
                Sizes),
    erlang:display({"C", Results}).

mult_C_exec_time(N, Size) ->
    M1 = utils:generateRandMat(Size, Size),
    M2 = utils:generateRandMat(Size, Size),
    Mat1 = numerl:matrix(M1),
    Mat2 = numerl:matrix(M2),
    {Time, _} = timer:tc(numerl, dot, [Mat1, Mat2]),
    if N == 1 ->
           [Time];
       true ->
           [Time | mult_C_exec_time(N - 1, Size)]
    end.

performance_erl() ->
    timer:sleep(100),
    Sizes = [10,50,100,200],%,1000,2000],
    Results =
        lists:map(fun(Size) ->
                     Times = mult_erl_exec_time(100, Size),
                     stats(Times)
                  end,
                  Sizes),
    erlang:display({erl, Results}).

mult_erl_exec_time(N, Size) ->
    M1 = utils:generateRandMat(Size, Size),
    M2 = utils:generateRandMat(Size, Size),
    {Time, _} = timer:tc(mat, '*', [M1, M2]),
    if N == 1 ->
           [Time];
       true ->
           [Time | mult_erl_exec_time(N - 1, Size)]
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