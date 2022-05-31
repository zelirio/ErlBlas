-module(sequential).

-export([dgemm/7, add/2, daxpy/3, dscal/2, mult/2]).

dgemm(ATransp, BTransp, Alpha, M1, M2, Beta, C) ->
    if ATransp ->
           A = tr(M1);
       true ->
           A = M1
    end,
    if BTransp ->
           B = M2;
       true ->
           B = tr(M2)
    end,
    if Beta /= 1.0 ->
           dscal(Beta, C);
       true ->
           skip
    end,
    lists:zipwith(fun(RowA, RowC) ->
                     lists:zipwith(fun(RowB, ElemC) ->
                                      lists:zipwith(fun(ElemA, ElemB) ->
                                                       numerl:dgemm(if ATransp ->
                                                                           1;
                                                                       true ->
                                                                           0
                                                                    end,
                                                                    if BTransp ->
                                                                           1;
                                                                       true ->
                                                                           0
                                                                    end,
                                                                    Alpha,
                                                                    ElemA,
                                                                    ElemB,
                                                                    1.0,
                                                                    ElemC)
                                                    end,
                                                    RowA,
                                                    RowB)
                                   end,
                                   B,
                                   RowC)
                  end,
                  A,
                  C).

add(M1, M2) ->
    M3 = erlBlas:copy(M1),
    daxpy(1.0, M2, M3),
    M3.

daxpy(Alpha, X, Y) ->
    utils:element_wise_op(fun(A, B) -> numerl:daxpy(Alpha, A, B) end, X, Y).

dscal(Alpha, X) ->
    utils:matrix_operation(fun(A) -> numerl:dscal(Alpha, A) end, X).

tr(M) ->
    tr(M, []).

tr([[] | _], Rows) ->
    lists:reverse(Rows);
tr(M, Rows) ->
    {Row, Cols} = tr(M, [], []),
    tr(Cols, [Row | Rows]).

tr([], Col, Cols) ->
    {lists:reverse(Col), lists:reverse(Cols)};
tr([[H | T] | Rows], Col, Cols) ->
    tr(Rows, [H | Col], [T | Cols]).

mult(M1, M2) ->
    multT(M1, tr(M2)).

multT(M1, M2) ->
    [[utils:lineSum(
          lists:zipwith(fun(A, B) -> numerl:dot(A, B) end, Li, Cj))
      || Cj <- M2]
     || Li <- M1].
