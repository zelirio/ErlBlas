-module(mult_conc_test_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("eunit/include/eunit.hrl").

performance_test_() ->
    {timeout,
     1000,
     fun() ->
        MaxLengths = [50],
        lists:map(fun(MaxLength) ->
                     erlBlas:set_max_length(MaxLength),
                     erlang:display({max_length, erlBlas:get_max_length()}),
                     performance(),
                     performance_conc()
                  end,
                  MaxLengths)
     end}.

performance_conc() ->
    timer:sleep(100),
    %mult_conc_exec_time(10,100),
    Sizes = [10, 50, 100, 200, 350, 500],%,1000,2000],
    Results =
        lists:map(fun(Size) ->
                     Times = mult_conc_exec_time(10, Size),
                     stats(Times)
                  end,
                  Sizes),
    erlang:display({conc, Results}).

mult_conc_exec_time(N, Size) ->
    M1 = utils:generateRandMat(Size, Size),
    M2 = utils:generateRandMat(Size, Size),
    Mat1 = erlBlas:matrix(M1),
    Mat2 = erlBlas:matrix(M2),
    {Time, _} = timer:tc(erlBlas, mult, [Mat1, Mat2]),
    if N == 1 ->
           [Time];
       true ->
           [Time | mult_conc_exec_time(N - 1, Size)]
    end.

performance() ->
    timer:sleep(100),
    %mult_exec_time(10,100),
    Sizes = [10, 50, 100, 200, 350, 500],%,1000,2000],
    Results =
        lists:map(fun(Size) ->
                     Times = mult_exec_time(10, Size),
                     stats(Times)
                  end,
                  Sizes),
    erlang:display({seq, Results}).

mult_exec_time(N, Size) ->
    M1 = utils:generateRandMat(Size, Size),
    M2 = utils:generateRandMat(Size, Size),
    Mat1 = erlBlas:matrix(M1),
    Mat2 = erlBlas:matrix(M2),
    {Time, _} = timer:tc(sequential, mult, [Mat1, Mat2]),
    if N == 1 ->
           [Time];
       true ->
           [Time | mult_exec_time(N - 1, Size)]
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
