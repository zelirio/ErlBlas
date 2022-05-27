-module(erlBlas).

-compile({no_auto_import, [get/1, put/2]}).

-import(persistent_term, [get/1, put/2]).

-on_load benchmark/0.

-import(utils,
        [split4/1,
         recompose4/4,
         appendEach/2,
         appendEachList/1,
         appendList/1,
         generateRandMat/2,
         splitLine/4,
         split4/3,
         element_wise_op/3,
         display_mat/1,
         lineSum/1,
         element_wise_op_conc/3]).

-export([add/2, sub/2, mult/2, inv/1, zeros/2, matrix/1, eye/1, toErl/1, equals/2,
         first_try_benchmark/0, test_time/2, set_max_length/1, copy/1, copy_shape/1]).
-export([dgemm/7, daxpy/3, dscal/2]).
-export([tr/1, transpose/1, get_max_length/0, get_shape/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% MATRIX GENERATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Creates a zero matrix of size (N x M)
zeros(N, M) ->
    MAX_LENGTH = get(max_length),
    RestN = N rem MAX_LENGTH,
    DivN = N div MAX_LENGTH,

    RestM = M rem MAX_LENGTH,
    DivM = M div MAX_LENGTH,

    if N =< MAX_LENGTH ->
           if M =< MAX_LENGTH ->
                  [[numerl:zeros(N, M)]];
              true ->
                  if RestM == 0 ->
                         A = zeros(N, MAX_LENGTH),
                         B = zeros(N, (DivM - 1) * MAX_LENGTH),
                         appendEach(A, B);
                     true ->
                         A = zeros(N, RestM),
                         B = zeros(N, DivM * MAX_LENGTH),
                         appendEach(A, B)
                  end
           end;
       true ->
           if M =< MAX_LENGTH ->
                  if RestN == 0 ->
                         A = zeros(MAX_LENGTH, M),
                         B = zeros((DivN - 1) * MAX_LENGTH, M),
                         lists:append(A, B);
                     true ->
                         A = zeros(RestN, M),
                         B = zeros(DivN * MAX_LENGTH, M),
                         lists:append(A, B)
                  end;
              true ->
                  if RestM == 0 ->
                         if RestN == 0 ->
                                Mat1 = zeros(MAX_LENGTH, MAX_LENGTH),
                                Mat2 = zeros(MAX_LENGTH, (DivM - 1) * MAX_LENGTH),
                                Mat3 = zeros((DivN - 1) * MAX_LENGTH, MAX_LENGTH),
                                Mat4 = zeros((DivN - 1) * MAX_LENGTH, (DivM - 1) * MAX_LENGTH),
                                recompose4(Mat1, Mat2, Mat3, Mat4);
                            true ->
                                Mat1 = zeros(RestN, MAX_LENGTH),
                                Mat2 = zeros(RestN, (DivM - 1) * MAX_LENGTH),
                                Mat3 = zeros(DivN * MAX_LENGTH, MAX_LENGTH),
                                Mat4 = zeros(DivN * MAX_LENGTH, (DivM - 1) * MAX_LENGTH),
                                recompose4(Mat1, Mat2, Mat3, Mat4)
                         end;
                     true ->
                         if RestN == 0 ->
                                Mat1 = zeros(MAX_LENGTH, RestM),
                                Mat2 = zeros(MAX_LENGTH, DivM * MAX_LENGTH),
                                Mat3 = zeros((DivN - 1) * MAX_LENGTH, RestM),
                                Mat4 = zeros((DivN - 1) * MAX_LENGTH, DivM * MAX_LENGTH),
                                recompose4(Mat1, Mat2, Mat3, Mat4);
                            true ->
                                Mat1 = zeros(RestN, RestM),
                                Mat2 = zeros(RestN, DivM * MAX_LENGTH),
                                Mat3 = zeros(DivN * MAX_LENGTH, RestM),
                                Mat4 = zeros(DivN * MAX_LENGTH, DivM * MAX_LENGTH),
                                recompose4(Mat1, Mat2, Mat3, Mat4)
                         end
                  end
           end
    end.

% Returns an Identity matrix of size (N x N)
eye(N) ->
    MAX_LENGTH = get(max_length),
    RestN = N rem MAX_LENGTH,
    DivN = N div MAX_LENGTH,
    if N =< MAX_LENGTH ->
           [[numerl:eye(N)]];
       true ->
           if RestN == 0 ->
                  Mat1 = eye(MAX_LENGTH),
                  Mat2 = zeros(MAX_LENGTH, (DivN - 1) * MAX_LENGTH),
                  Mat3 = zeros((DivN - 1) * MAX_LENGTH, MAX_LENGTH),
                  Mat4 = eye((DivN - 1) * MAX_LENGTH),
                  recompose4(Mat1, Mat2, Mat3, Mat4);
              true ->
                  Mat1 = eye(RestN),
                  Mat2 = zeros(RestN, DivN * MAX_LENGTH),
                  Mat3 = zeros(DivN * MAX_LENGTH, RestN),
                  Mat4 = eye(DivN * MAX_LENGTH),
                  recompose4(Mat1, Mat2, Mat3, Mat4)
           end
    end.

% Take a numeric matrix in Erlang format (list of lists of numbers) and returns a matrix in numerlplus format (list of lists of numerl submatrices)
matrix(Mat) ->
    N = length(Mat),
    M = length(lists:nth(1, Mat)),
    matrix(Mat, N, M).

matrix(Mat, N, M) ->
    MAX_LENGTH = get(max_length),

    if N =< MAX_LENGTH, M =< MAX_LENGTH ->
           [[numerl:matrix(Mat)]];
       N =< MAX_LENGTH, M > MAX_LENGTH ->
           RestM = M rem MAX_LENGTH,
           DivM = M div MAX_LENGTH,
           if RestM == 0 ->
                  {L, R} = splitLine(Mat, [], [], MAX_LENGTH),
                  A = matrix(L, N, MAX_LENGTH),
                  B = matrix(R, N, (DivM - 1) * MAX_LENGTH),
                  appendEach(A, B);
              true ->
                  {L, R} = splitLine(Mat, [], [], RestM),
                  A = matrix(L, N, RestM),
                  B = matrix(R, N, DivM * MAX_LENGTH),
                  appendEach(A, B)
           end;
       N > MAX_LENGTH, M =< MAX_LENGTH ->
           RestN = N rem MAX_LENGTH,
           DivN = N div MAX_LENGTH,
           if RestN == 0 ->
                  {M1, M2} = lists:split(MAX_LENGTH, Mat),
                  A = matrix(M1, MAX_LENGTH, M),
                  B = matrix(M2, (DivN - 1) * MAX_LENGTH, M),
                  lists:append(A, B);
              true ->
                  {M1, M2} = lists:split(RestN, Mat),
                  A = matrix(M1, RestN, M),
                  B = matrix(M2, DivN * MAX_LENGTH, M),
                  lists:append(A, B)
           end;
       N > MAX_LENGTH, M > MAX_LENGTH ->
           RestN = N rem MAX_LENGTH,
           DivN = N div MAX_LENGTH,
           RestM = M rem MAX_LENGTH,
           DivM = M div MAX_LENGTH,

           if RestM == 0 ->
                  if RestN == 0 ->
                         {A, B, C, D} = split4(Mat, MAX_LENGTH, MAX_LENGTH),
                         Mat1 = matrix(A, MAX_LENGTH, MAX_LENGTH),
                         Mat2 = matrix(B, MAX_LENGTH, (DivM - 1) * MAX_LENGTH),
                         Mat3 = matrix(C, (DivN - 1) * MAX_LENGTH, MAX_LENGTH),
                         Mat4 = matrix(D, (DivN - 1) * MAX_LENGTH, (DivM - 1) * MAX_LENGTH),
                         recompose4(Mat1, Mat2, Mat3, Mat4);
                     true ->
                         {A, B, C, D} = split4(Mat, RestN, MAX_LENGTH),
                         Mat1 = matrix(A, RestN, MAX_LENGTH),
                         Mat2 = matrix(B, RestN, (DivM - 1) * MAX_LENGTH),
                         Mat3 = matrix(C, DivN * MAX_LENGTH, MAX_LENGTH),
                         Mat4 = matrix(D, DivN * MAX_LENGTH, (DivM - 1) * MAX_LENGTH),
                         recompose4(Mat1, Mat2, Mat3, Mat4)
                  end;
              true ->
                  if RestN == 0 ->
                         {A, B, C, D} = split4(Mat, MAX_LENGTH, RestM),
                         Mat1 = matrix(A, MAX_LENGTH, RestM),
                         Mat2 = matrix(B, MAX_LENGTH, DivM * MAX_LENGTH),
                         Mat3 = matrix(C, (DivN - 1) * MAX_LENGTH, RestM),
                         Mat4 = matrix(D, (DivN - 1) * MAX_LENGTH, DivM * MAX_LENGTH),
                         recompose4(Mat1, Mat2, Mat3, Mat4);
                     true ->
                         {A, B, C, D} = split4(Mat, RestN, RestM),
                         Mat1 = matrix(A, RestN, RestM),
                         Mat2 = matrix(B, RestN, DivM * MAX_LENGTH),
                         Mat3 = matrix(C, DivN * MAX_LENGTH, RestM),
                         Mat4 = matrix(D, DivN * MAX_LENGTH, DivM * MAX_LENGTH),
                         recompose4(Mat1, Mat2, Mat3, Mat4)
                  end
           end
    end.

copy(M) ->
    utils:matrix_operation(fun numerl:copy/1, M).

copy_shape(M) ->
    utils:matrix_operation(fun numerl:copy_shape/1, M).

get_shape(M) ->
    H = get_height(M, 0),
    W = get_width(lists:nth(1, M), 0),
    {H, W}.

get_height(M, N) ->
    case M of
        [] ->
            N;
        [H | T] ->
            {L, _} =
                numerl:get_shape(
                    lists:nth(1, H)),
            get_height(T, N + L);
        _ ->
            error("get_height: invalid matrix")
    end.

get_width(M, N) ->
    case M of
        [] ->
            N;
        [H | T] ->
            {_, W} = numerl:get_shape(H),
            get_width(T, N + W);
        _ ->
            error("get_width: invalid matrix")
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% NUMERIC OPERATIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Returns the result of the addition of the two matrices in argument, in numerlplus format
add(M1, M2) ->
    M3 = copy(M1),
    daxpy(1.0, M2, M3),
    M3.

% Returns the result of the substraction of the two matrices in argument, in numerlplus format
sub(M1, M2) ->
    M3 = copy(M1),
    daxpy(-1.0, M2, M3),
    M3.

% Returns the result of the multiplication of the two matrices in argument, in numerlplus format
mult(M1, M2) ->
    {R1, C1} = get_shape(M1),
    {C1, C2} = get_shape(M2),
    M3 = zeros(R1, C2),
    dgemm(false, false, 1.0, M1, M2, 0.0, M3),
    M3.

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

% Returns the result of the multiplication of the matrix M by the scalar Const
scal(Const, M) ->
    R = copy(M),
    dscal(Const, R),
    R.

% Returns the inverse of the matrix given, in numerlplus format
inv(M1) ->
    Len = length(M1),
    if Len > 1 ->
           {A, B, C, D} = split4(M1),
           InvA = inv(A),
           InvAB = mult(InvA, B),
           CInvA = mult(C, InvA),
           C4 = inv(sub(D, mult(C, InvAB))), % inv(D-C InvA B)
           Big = mult(C4, CInvA),
           C3 = scal(-1, Big),
           C2 = scal(-1, mult(InvAB, C4)),
           C1 = add(InvA, mult(InvAB, Big)),
           recompose4(C1, C2, C3, C4);
       true ->
           [[Bin]] = M1,
           [[numerl:inv(Bin)]]
    end.

% Returns the transpose of the matrix given, in numerlplus format
transpose(M) ->
    Tr = tr(M),
    utils:matrix_operation(fun numerl:transpose/1, Tr).

% Takes a matrix in numerlplus format and return the same matrix in erlang format (list of lists of numbers)
toErl(M) ->
    appendList(lists:map(fun(Row) ->
                            appendEachList(lists:map(fun(Elem) -> numerl:mtfl(Elem) end, Row))
                         end,
                         M)).

% Returns true if the elements of the two matrix given (in numerlplus format) are all the same, false otherwise.
equals(M1, M2) ->
    Eq = element_wise_op(fun numerl:equals/2, M1, M2),
    lists:all(fun(Row) -> lists:all(fun(B) -> B end, Row) end, Eq).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% BENCHMARK %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_max_length() ->
    get(max_length).

set_max_length(N) ->
    put(max_length, N).

benchmark() ->
    put(max_length, 50).

first_try_benchmark() ->
    First_max = lists:min([round_one_benchmark(5) || _ <- lists:seq(1, 5)]),
    %erlang:display({first_max, First_max}),
    Second_max =
        lists:nth(2, lists:sort([round_two_benchmark(First_max) || _ <- lists:seq(1, 20)])),
    %erlang:display({second_max, Second_max}),
    put(max_length, Second_max).

round_one_benchmark(N) ->
    Passed = test_time(N, false),
    if Passed ->
           round_one_benchmark(N * 2);
       true ->
           N div 2
    end.

round_two_benchmark(N) ->
    Passed = test_time(N, false),
    if Passed ->
           round_two_benchmark(round(N * 1.2));
       true ->
           round(N / 1.2)
    end.

test_time(N, true) ->
    timer:sleep(100),
    erlang:display(hello),
    M = generateRandMat(N, N),
    Mat = numerl:matrix(M),
    TimesValues = [timer:tc(numerl, dot, [Mat, Mat]) || _ <- lists:seq(1, 50)],
    Times = [Time || {Time, _} <- TimesValues],
    Test = lists:search(fun(Time) -> Time > 1000 end, Times),
    case Test of
        {value, _} ->
            %erlang:display({N, Time}),
            false;
        _ ->
            true
    end;
test_time(N, false) ->
    M = generateRandMat(N, N),
    M2 = generateRandMat(N, N),
    Mat = numerl:matrix(M),
    Mat2 = numerl:matrix(M2),
    C = numerl:zeros(N, N),
    {Time, _} = timer:tc(numerl, dgemm, [1, 1, 2.5, Mat, Mat2, 3.5, C]),
    if Time < 1000 ->
           %erlang:display({ok, N, Time}),
           true;
       true ->
           %erlang:display({foire, N, Time}),
           false
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% BLAS INTERFACE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    PID = self(),
    PIDs =
        lists:zipwith(fun(RowA, RowC) ->
                         spawn(fun() ->
                                  ParentPID = self(),
                                  PidList =
                                      lists:zipwith(fun(RowB, ElemC) ->
                                                       spawn(fun() ->
                                                                lists:zipwith(fun(ElemA, ElemB) ->
                                                                                 numerl:dgemm(if
                                                                                                  ATransp ->
                                                                                                      1;
                                                                                                  true ->
                                                                                                      0
                                                                                              end,
                                                                                              if
                                                                                                  BTransp ->
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
                                                                              RowB),
                                                                ParentPID ! {finished, self()}
                                                             end)
                                                    end,
                                                    B,
                                                    RowC),
                                  lists:map(fun(Elem) ->
                                               receive
                                                   {finished, Elem} ->
                                                       ok
                                               end
                                            end,
                                            PidList),
                                  PID ! {finished, self()}
                               end)
                      end,
                      A,
                      C),

    lists:map(fun(Elem) ->
                 receive
                     {finished, Elem} ->
                         ok
                 end
              end,
              PIDs).

daxpy(Alpha, X, Y) ->
    utils:element_wise_op_conc3(fun(A, B) -> numerl:daxpy(Alpha, A, B) end, X, Y).

dscal(Alpha, X) ->
    utils:matrix_operation(fun(A) -> numerl:dscal(Alpha, A) end, X).
