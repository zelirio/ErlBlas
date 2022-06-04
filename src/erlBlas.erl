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

    if N rem MAX_LENGTH == 0 ->  
        ModN = MAX_LENGTH,
        RowMultiple = N div MAX_LENGTH - 1;
    true ->
        ModN = N rem MAX_LENGTH,
        RowMultiple = N div MAX_LENGTH
    end,
    
    if M rem MAX_LENGTH == 0 ->  
        ModM = MAX_LENGTH,
        ColMultiple = M div MAX_LENGTH - 1;
    true ->
        ModM = M rem MAX_LENGTH,
        ColMultiple = M div MAX_LENGTH
    end,


    if  
        N =< MAX_LENGTH, M =< MAX_LENGTH ->
            [[numerl:zeros(N, M)]];
        N =< MAX_LENGTH, M > MAX_LENGTH ->
            A = zeros(N, ModM),
            B = zeros(N, ColMultiple * MAX_LENGTH),
            appendEach(A, B);
        N > MAX_LENGTH, M =< MAX_LENGTH ->
            A = zeros(ModN, M),
            C = zeros(RowMultiple * MAX_LENGTH, M),
            lists:append(A, C);
        N > MAX_LENGTH, M > MAX_LENGTH ->
            A = zeros(ModN, ModM),
            B = zeros(ModN, ColMultiple * MAX_LENGTH),
            C = zeros(RowMultiple * MAX_LENGTH, ModM),
            D = zeros(RowMultiple* MAX_LENGTH, ColMultiple * MAX_LENGTH),
            recompose4(A,B,C,D)
    end.

% Returns an Identity matrix of size (N x N)
eye(N) ->
    MAX_LENGTH = get(max_length),
    
    if N rem MAX_LENGTH == 0 ->  
        ModN = MAX_LENGTH,
        RowMultiple = N div MAX_LENGTH - 1;
    true ->
        ModN = N rem MAX_LENGTH,
        RowMultiple = N div MAX_LENGTH
    end,

    if 
        N =< MAX_LENGTH ->
            [[numerl:eye(N)]];
        N > MAX_LENGTH ->
            A = eye(ModN),
            B = zeros(ModN, RowMultiple * MAX_LENGTH),
            C = zeros(RowMultiple * MAX_LENGTH, ModN),
            D = eye(RowMultiple * MAX_LENGTH),
            recompose4(A,B,C,D)
    end.

% Take a numeric matrix in Erlang format (list of lists of numbers) and returns a matrix in numerlplus format (list of lists of numerl submatrices)
matrix(Mat) ->
    N = length(Mat),
    M = length(lists:nth(1, Mat)),
    matrix(Mat, N, M).

matrix(Mat, N, M) ->
    MAX_LENGTH = get(max_length),

    if N rem MAX_LENGTH == 0 ->  
        ModN = MAX_LENGTH,
        RowMultiple = N div MAX_LENGTH - 1;
    true ->
        ModN = N rem MAX_LENGTH,
        RowMultiple = N div MAX_LENGTH
    end,
    
    if M rem MAX_LENGTH == 0 ->  
        ModM = MAX_LENGTH,
        ColMultiple = M div MAX_LENGTH - 1;
    true ->
        ModM = M rem MAX_LENGTH,
        ColMultiple = M div MAX_LENGTH
    end,

    if  
        N =< MAX_LENGTH, M =< MAX_LENGTH ->
            [[numerl:matrix(Mat)]];
        N =< MAX_LENGTH, M > MAX_LENGTH ->
            {L, R} = splitLine(Mat, [], [], ModM),
            A = matrix(L, N, ModM),
            B = matrix(R, N, ColMultiple * MAX_LENGTH),
            appendEach(A, B);
        N > MAX_LENGTH, M =< MAX_LENGTH ->
            {U, L} = lists:split(ModN, Mat),
            A = matrix(U, ModN, M),
            C = matrix(L, RowMultiple * MAX_LENGTH, M),
            lists:append(A, C);
        N > MAX_LENGTH, M > MAX_LENGTH ->
            {UL,UR,LL,LR} = split4(Mat, ModN, ModM),
            A = matrix(UL, ModN, ModM),
            B = matrix(UR, ModN, ColMultiple * MAX_LENGTH),
            C = matrix(LL, RowMultiple * MAX_LENGTH, ModM),
            D = matrix(LR, RowMultiple * MAX_LENGTH, ColMultiple * MAX_LENGTH),
            recompose4(A,B,C,D)
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
    Second_max =
        lists:nth(2, lists:sort([round_two_benchmark(First_max) || _ <- lists:seq(1, 20)])),
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
           true;
       true ->
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
                                                                                 numerl:dgemm(if ATransp -> 1; true -> 0 end,
                                                                                              if BTransp -> 1; true -> 0 end,
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
