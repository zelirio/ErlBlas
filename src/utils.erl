-module(utils).

-export([generateRandMat/2, append/1, appendEach/1, appendEach/2, appendEachList/1,
         appendList/1, splitLine/3, split4/1, recompose4/4, recompose4/1, split4/3, splitLine/4,
         element_wise_op/3, display_mat/1, lineSum/1]).
-export([element_wise_op_conc/3, sendResult/4, element_wise_add_conc/2,
         element_wise_op_conc2/3, element_wise_op_conc3/3,  matrix_operation/2]).

generateRandMat(0, _) ->
    [];
generateRandMat(Dim1, Dim2) ->
    [generateRandVect(Dim2) | generateRandMat(Dim1 - 1, Dim2)].

generateRandVect(0) ->
    [];
generateRandVect(Dim2) ->
    [rand:uniform(5) | generateRandVect(Dim2 - 1)].

splitLine(M1, Acc1, Acc2) ->
    case M1 of
        [] ->
            {lists:reverse(Acc1), lists:reverse(Acc2)};
        [H | T] ->
            {A1, A2} = lists:split(trunc(length(H) / 2), H),
            splitLine(T, [A1 | Acc1], [A2 | Acc2])
    end.

splitLine(M1, Acc1, Acc2, N) ->
    case M1 of
        [] ->
            {lists:reverse(Acc1), lists:reverse(Acc2)};
        [H | T] ->
            {A1, A2} = lists:split(N, H),
            splitLine(T, [A1 | Acc1], [A2 | Acc2], N)
    end.

split4(M1) ->
    {A1, A2} = splitLine(M1, [], []),
    {Xa, Xc} = lists:split(trunc(length(A1) / 2), A1),
    {Xb, Xd} = lists:split(trunc(length(A2) / 2), A2),
    {Xa, Xb, Xc, Xd}.

split4(M1, N, M) ->
    {A1, A2} = splitLine(M1, [], [], M),
    {Xa, Xc} = lists:split(N, A1),
    {Xb, Xd} = lists:split(N, A2),
    {Xa, Xb, Xc, Xd}.

recompose4(M1, M2, M3, M4) ->
    lists:append(appendEach(M1, M2), appendEach(M3, M4)).

recompose4({Pid, ID}) ->
    receive
        {a, A} ->
            receive
                {b, B} ->
                    receive
                        {c, C} ->
                            receive
                                {d, D} ->
                                    Result = recompose4(A, B, C, D),
                                    Pid ! {ID, Result}
                            end
                    end
            end
    end.

appendEach({Pid, ID}) ->
    receive
        {a, A} ->
            receive
                {b, B} ->
                    Result = appendEach(A, B),
                    %erlang:display({sent, Pid, ID, dims(Result)}),
                    Pid ! {ID, Result}
            end
    end.

append({Pid, ID}) ->
    receive
        {a, A} ->
            receive
                {b, B} ->
                    Result = lists:append(A, B),
                    %erlang:display({sent, Pid, ID, dims(Result)}),
                    Pid ! {ID, Result}
            end
    end.

appendEach(M1, M2) ->
    case {M1, M2} of
        {[], []} ->
            [];
        {[H1 | T1], [H2 | T2]} ->
            [lists:append(H1, H2) | appendEach(T1, T2)]
    end.

appendEachList(L) ->
    case L of
        [H] ->
            H;
        [H | T] ->
            appendEach(H, appendEachList(T))
    end.

appendList(L) ->
    case L of
        [H] ->
            H;
        [H | T] ->
            lists:append(H, appendList(T))
    end.

display_mat(M) ->
    lists:map(fun(Row) ->
                 lists:map(fun(Elem) ->
                              erlang:display(
                                  numerl:mtfli(Elem))
                           end,
                           Row)
              end,
              M).

element_wise_op(Op, M1, M2) ->
    lists:zipwith(fun(L1, L2) -> lists:zipwith(fun(E1, E2) -> Op(E1, E2) end, L1, L2) end,
                  M1,
                  M2).

element_wise_op2(Op, M1, M2) ->
    lists:zipwith(fun(L1, L2) ->
                     lists:zipwith(fun(E1, E2) ->
                                      {Time, Value} = timer:tc(Op, [E1, E2]),
                                      erlang:display(Time),
                                      Value
                                   end,
                                   L1,
                                   L2)
                  end,
                  M1,
                  M2).

element_wise_op_conc(Op, M1, M2) ->
    PidMat =
        lists:zipwith(fun(L1, L2) ->
                         lists:zipwith(fun(E1, E2) -> spawn(utils, sendResult, [Op, E1, E2, self()])
                                       end,
                                       L1,
                                       L2)
                      end,
                      M1,
                      M2),
    lists:map(fun(Row) ->
                 lists:map(fun(Pid) ->
                              receive
                                  {Result, Pid} ->
                                      Result
                              end
                           end,
                           Row)
              end,
              PidMat).

element_wise_op_conc2(Op, M1, M2) ->
    ParentPID = self(),
    PidList =
        lists:zipwith(fun(Row1, Row2) ->
                         spawn(fun() ->
                                  Result =
                                      lists:zipwith(fun(Elem1, Elem2) ->
                                                       ParPID = self(),
                                                       Pidipid =
                                                           spawn(fun() ->
                                                                    Res = Op(Elem1, Elem2),
                                                                    ParPID ! {Res, self()}
                                                                 end),
                                                       receive
                                                           {Res, Pidipid} ->
                                                               Res
                                                       end
                                                    end,
                                                    Row1,
                                                    Row2),
                                  ParentPID ! {Result, self()}
                               end)
                      end,
                      M1,
                      M2),
    %erlang:display(PidList),
    lists:map(fun(Pid) ->
                 receive
                     {Result, Pid} ->
                         Result
                 end
              end,
              PidList).

element_wise_op_conc3(Op, M1, M2) ->
                ParentPID = self(),
                PidMat =
            lists:zipwith(fun(L1, L2) -> spawn(fun() -> Result = lists:zipwith(fun(E1, E2) -> Op(E1, E2) end,L1,L2), ParentPID ! {Result, self()} end) end,
                                  M1,
                                  M2),
                lists:map(fun(LinePid) ->
                            receive 
                                {Result, LinePid} ->
                                    Result
                            end 
                          end,
                          PidMat).

element_wise_add_conc(M1, M2) ->
    ParentPID = self(),
    PidMat =
        lists:zipwith(fun(L1, L2) ->
                         lists:zipwith(fun(E1, E2) ->
                                          spawn(fun() -> ParentPID ! {numerl:add(E1, E2), self()}
                                                end)
                                       end,
                                       L1,
                                       L2)
                      end,
                      M1,
                      M2),
    lists:map(fun(Row) ->
                 lists:map(fun(Pid) ->
                              receive
                                  {Result, Pid} ->
                                      Result
                              end
                           end,
                           Row)
              end,
              PidMat).

sendResult(Op, Elem1, Elem2, ParentPID) ->
    Value = Op(Elem1, Elem2),
    ParentPID ! {Value, self()}.

matrix_operation(Op, M) ->
    lists:map(fun(Row) -> lists:map(fun(A) -> Op(A) end, Row) end, M).

lineSum([H | T]) ->
    lineSum(T, H).

lineSum(List, Acc) ->
    case List of
        [H | T] ->
            lineSum(T, numerl:add(Acc, H));
        [] ->
            Acc
    end.

lineSum_conc([H | T]) ->
    lineSum(T, H).

lineSum_conc(List, Acc) ->
    PID = self(),
    case List of
        [H | T] ->
            Pid = spawn(fun() ->
                           Result = lineSum_conc(T, numerl:add(Acc, H)),
                           PID ! {Result, self()}
                        end);
        [] ->
            Pid = spawn(fun() -> PID ! {Acc, self()} end)
    end,
    receive
        {Return, Pid} ->
            Return
    end.
