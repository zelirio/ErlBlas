-module(block_mat).

-compile({no_auto_import,[get/1,put/2]}).
-import(persistent_term,[get/1,put/2]).
-on_load(benchmark/0).

-export([add/2, sub/2, mult/2, inv/1, zeros/2, matrix/1, eye/1, display_mat/1, toErl/1, first_try_benchmark/0, test_time/2,matrix_conc/1,zeros_conc/2]).
-export([append/1,appendEach/1,recompose4/1,matrix_conc/4,dims/1,zeros_conc/3]).

-type matrix() :: [[number(), ...], ...].

eye(N) ->
    MAX_LENGTH = get(max_length),
    RestN = N rem MAX_LENGTH,
    DivN = N div MAX_LENGTH,
    if N =< MAX_LENGTH ->
        [[numerl:eye(N)]];
    true ->
        if RestN == 0 ->
            Mat1 = eye(MAX_LENGTH),
            Mat2 = zeros(MAX_LENGTH, (DivN-1)*MAX_LENGTH),
            Mat3 = zeros((DivN-1)*MAX_LENGTH,MAX_LENGTH),
            Mat4 = eye((DivN-1)*MAX_LENGTH),
            recompose4(Mat1, Mat2, Mat3, Mat4);
        true ->
            Mat1 = eye(RestN),
            Mat2 = zeros(RestN, DivN*MAX_LENGTH),
            Mat3 = zeros(DivN*MAX_LENGTH,RestN),
            Mat4 = eye(DivN*MAX_LENGTH),
            recompose4(Mat1, Mat2, Mat3, Mat4)
        end
    end.

display_mat(M) ->
    lists:map(fun(Row) -> lists:map(fun(Elem)-> erlang:display(numerl:mtfli(Elem)) end, Row) end,M).


element_wise_op(Op, M1, M2) ->
    lists:zipwith(fun(L1, L2) -> lists:zipwith(Op, L1, L2) end, M1, M2).



matrix(Mat) ->
    N = length(Mat),
    M = length(lists:nth(1,Mat)),
    matrix(Mat, N, M).

matrix(Mat, N, M) ->
    MAX_LENGTH = get(max_length),

    if N =< MAX_LENGTH, M =< MAX_LENGTH ->
        [[numerl:matrix(Mat)]];

    N =< MAX_LENGTH, M > MAX_LENGTH ->
        RestM = M rem MAX_LENGTH,
        DivM = M div MAX_LENGTH,
        if(RestM ==0 ) ->
            {L, R} = splitLine(Mat,[],[], MAX_LENGTH),
            A = matrix(L, N,MAX_LENGTH), 
            B = matrix(R, N, (DivM -1)* MAX_LENGTH),
            appendEach(A, B);
        true ->
            %(3x26)
            {L, R} = splitLine(Mat,[],[], RestM),
            A = matrix(L, N,RestM),
            B = matrix(R, N, DivM* MAX_LENGTH),
            appendEach(A, B)
        end;

    N > MAX_LENGTH, M =< MAX_LENGTH ->
        RestN = N rem MAX_LENGTH,
        DivN = N div (MAX_LENGTH),
        if RestN == 0 ->
            %(25, 3)
            {M1, M2} = lists:split(MAX_LENGTH, Mat),
            A = matrix(M1, MAX_LENGTH, M), 
            B = matrix(M2, (DivN-1)*MAX_LENGTH, M),
            lists:append(A,B);
        true ->
            %(26, 3)
            {M1, M2} = lists:split(RestN, Mat),
            A = matrix(M1, RestN, M), 
            B = matrix(M2, DivN*MAX_LENGTH, M),
            lists:append(A,B)
        end;

    N > MAX_LENGTH, M > MAX_LENGTH ->
        RestN = N rem MAX_LENGTH,
        DivN = N div (MAX_LENGTH),
        RestM = M rem MAX_LENGTH,
        DivM = M div MAX_LENGTH,

        if RestM == 0 ->
            if RestN == 0 ->
                %(25, 25)
                {A, B, C, D} = split4(Mat, MAX_LENGTH, MAX_LENGTH),
                Mat1 = matrix(A, MAX_LENGTH, MAX_LENGTH),
                Mat2 = matrix(B, MAX_LENGTH, (DivM-1)*MAX_LENGTH),
                Mat3 = matrix(C, (DivN-1)*MAX_LENGTH, MAX_LENGTH),
                Mat4 = matrix(D, (DivN-1)*MAX_LENGTH, (DivM-1)*MAX_LENGTH),
                recompose4(Mat1, Mat2, Mat3, Mat4);
            true ->
                %(26, 25)
                {A, B, C, D} = split4(Mat, RestN, MAX_LENGTH),
                Mat1 = matrix(A, RestN, MAX_LENGTH),
                Mat2 = matrix(B, RestN, (DivM-1)*MAX_LENGTH),
                Mat3 = matrix(C, (DivN)*MAX_LENGTH, MAX_LENGTH),
                Mat4 = matrix(D, (DivN)*MAX_LENGTH, (DivM-1)*MAX_LENGTH),
                recompose4(Mat1, Mat2, Mat3, Mat4)
            end;
        true ->
            if RestN == 0 ->
                %(25, 26)
                {A, B, C, D} = split4(Mat, MAX_LENGTH, RestM),
                Mat1 = matrix(A, MAX_LENGTH, RestM),
                Mat2 = matrix(B, MAX_LENGTH, (DivM)*MAX_LENGTH),
                Mat3 = matrix(C, (DivN-1)*MAX_LENGTH, RestM),
                Mat4 = matrix(D, (DivN-1)*MAX_LENGTH, (DivM)*MAX_LENGTH),
                recompose4(Mat1, Mat2, Mat3, Mat4);
            true ->
                %(26, 26)
                {A, B, C, D} = split4(Mat, RestN, RestM),
                Mat1 = matrix(A, RestN, RestM),
                Mat2 = matrix(B, RestN, (DivM)*MAX_LENGTH),
                Mat3 = matrix(C, (DivN)*MAX_LENGTH, RestM),
                Mat4 = matrix(D, (DivN)*MAX_LENGTH, (DivM)*MAX_LENGTH),
                recompose4(Mat1, Mat2, Mat3, Mat4)
            end
        end
    end.
    
matrix_conc(Mat) ->
    N = length(Mat),
    M = length(lists:nth(1,Mat)),
    matrix_conc(Mat, N, M, {self(), res}),
    receive 
        {res, Result} ->
            Result
    end.

matrix_conc(Mat, N, M, {Pid, ID}) ->
    MAX_LENGTH = get(max_length),

    if N =< MAX_LENGTH, M =< MAX_LENGTH ->
        Result = [[numerl:matrix(Mat)]],
        Pid ! {ID, Result};

    N =< MAX_LENGTH, M > MAX_LENGTH ->
        RestM = M rem MAX_LENGTH,
        DivM = M div MAX_LENGTH,
        if(RestM ==0 ) ->
            {L, R} = splitLine(Mat,[],[], MAX_LENGTH),
            Append = spawn(block_mat,appendEach,[{self(), result}]),
            spawn(block_mat,matrix_conc,[L, N,MAX_LENGTH,{Append,a}]), 
            spawn(block_mat,matrix_conc,[R, N, (DivM -1)* MAX_LENGTH, {Append,b}]),
            receive 
                {result, Result} ->
                    Pid ! {ID,Result}
            end;
        true ->
            %(3x26)
            {L, R} = splitLine(Mat,[],[], RestM),
            Append = spawn(block_mat,appendEach,[{self(),result}]),
            spawn(block_mat,matrix_conc,[L, N,RestM,{Append,a}]),
            spawn(block_mat,matrix_conc,[R, N, DivM* MAX_LENGTH,{Append,b}]),
            receive 
                {result, Result} ->
                    Pid ! {ID,Result}
            end
        end;

    N > MAX_LENGTH, M =< MAX_LENGTH ->
        RestN = N rem MAX_LENGTH,
        DivN = N div (MAX_LENGTH),
        if RestN == 0 ->
            %(25, 3)
            {M1, M2} = lists:split(MAX_LENGTH, Mat),
            Append = spawn(block_mat,append,[{self(), result}]),
            spawn(block_mat,matrix_conc,[M1, MAX_LENGTH, M, {Append,a}]), 
            spawn(block_mat,matrix_conc,[M2, (DivN-1)*MAX_LENGTH, M, {Append,b}]),
            receive 
                {result, Result} ->
                    Pid ! {ID,Result}
            end;
            
        true ->
            %(26, 3)
            {M1, M2} = lists:split(RestN, Mat),
            Append = spawn(block_mat,append,[{self(),result}]),
            spawn(block_mat,matrix_conc,[M1, RestN, M,{Append, a}]), 
            spawn(block_mat,matrix_conc,[M2, DivN*MAX_LENGTH, M, {Append,b}]),
            receive 
                {result, Result} ->
                    Pid ! {ID,Result}
            end
        end;

    N > MAX_LENGTH, M > MAX_LENGTH ->
        RestN = N rem MAX_LENGTH,
        DivN = N div (MAX_LENGTH),
        RestM = M rem MAX_LENGTH,
        DivM = M div MAX_LENGTH,

        if RestM == 0 ->
            if RestN == 0 ->
                %(25, 25)
                {A, B, C, D} = split4(Mat, MAX_LENGTH, MAX_LENGTH),
                Recompose = spawn(block_mat,recompose4,[{self(),result}]),
                spawn(block_mat,matrix_conc,[A, MAX_LENGTH, MAX_LENGTH,{Recompose, a}]),
                spawn(block_mat,matrix_conc,[B, MAX_LENGTH, (DivM-1)*MAX_LENGTH,{Recompose, b}]),
                spawn(block_mat,matrix_conc,[C, (DivN-1)*MAX_LENGTH, MAX_LENGTH,{Recompose, c}]),
                spawn(block_mat,matrix_conc,[D, (DivN-1)*MAX_LENGTH, (DivM-1)*MAX_LENGTH,{Recompose, d}]),
                receive 
                {result, Result} ->
                    Pid ! {ID,Result}
                end;
            true ->
                %(26, 25)
                {A, B, C, D} = split4(Mat, RestN, MAX_LENGTH),
                Recompose = spawn(block_mat,recompose4,[{self(),result}]),
                spawn(block_mat,matrix_conc,[A, RestN, MAX_LENGTH,{Recompose, a}]),
                spawn(block_mat,matrix_conc,[B, RestN, (DivM-1)*MAX_LENGTH,{Recompose, b}]),
                spawn(block_mat,matrix_conc,[C, (DivN)*MAX_LENGTH, MAX_LENGTH,{Recompose, c}]),
                spawn(block_mat,matrix_conc,[D, (DivN)*MAX_LENGTH, (DivM-1)*MAX_LENGTH,{Recompose, d}]),
                receive 
                {result, Result} ->
                    Pid ! {ID,Result}
                end
            end;
        true ->
            if RestN == 0 ->
                %(25, 26)
                {A, B, C, D} = split4(Mat, MAX_LENGTH, RestM),
                Recompose = spawn(block_mat,recompose4,[{self(),result}]),
                spawn(block_mat,matrix_conc,[A, MAX_LENGTH, RestM,{Recompose, a}]),
                spawn(block_mat,matrix_conc,[B, MAX_LENGTH, (DivM)*MAX_LENGTH,{Recompose, b}]),
                spawn(block_mat,matrix_conc,[C, (DivN-1)*MAX_LENGTH, RestM,{Recompose, c}]),
                spawn(block_mat,matrix_conc,[D, (DivN-1)*MAX_LENGTH, (DivM)*MAX_LENGTH,{Recompose, d}]),
                receive 
                {result, Result} ->
                    Pid ! {ID,Result}
                end;
            true ->
                %(26, 26)
                {A, B, C, D} = split4(Mat, RestN, RestM),
                Recompose = spawn(block_mat,recompose4,[{self(),result}]),
                spawn(block_mat,matrix_conc,[A, RestN, RestM,{Recompose, a}]),
                spawn(block_mat,matrix_conc,[B, RestN, (DivM)*MAX_LENGTH,{Recompose, b}]),
                spawn(block_mat,matrix_conc,[C, (DivN)*MAX_LENGTH, RestM,{Recompose, c}]),
                spawn(block_mat,matrix_conc,[D, (DivN)*MAX_LENGTH, (DivM)*MAX_LENGTH,{Recompose, d}]),
                receive 
                {result, Result} ->
                    Pid ! {ID,Result}
                end
            end
        end
    end.

zeros(N,M) ->
    MAX_LENGTH = get(max_length),
    RestN = N rem MAX_LENGTH,
    DivN = N div (MAX_LENGTH),

    RestM = M rem MAX_LENGTH,
    DivM = M div (MAX_LENGTH),
    
    if N =< MAX_LENGTH ->
        if M =< MAX_LENGTH ->
            %(3x3)
            [[numerl:zeros(N,M)]];
        true ->
            if RestM == 0 ->
                %(3x25) 
                A = zeros(N,MAX_LENGTH), 
                B = zeros(N,(DivM-1)*MAX_LENGTH), 
                appendEach(A,B);
            true ->
                %(3x26)
                A = zeros(N,RestM),
                B = zeros(N,(DivM)*MAX_LENGTH),
                appendEach(A,B)
            end
        end;
    true ->
        if M =< MAX_LENGTH ->
            if RestN == 0 ->
                %(25x3)
                A = zeros(MAX_LENGTH, M), 
                B = zeros((DivN-1)*MAX_LENGTH,M),
                lists:append(A,B);
            true ->
                %(26x3)
                A = zeros(RestN, M), 
                B = zeros((DivN)*MAX_LENGTH,M),
                lists:append(A,B)
            end;
        true ->
            if RestM == 0 ->
                if RestN == 0 ->
                    %(25x25)
                    Mat1 = zeros(MAX_LENGTH,MAX_LENGTH),
                    Mat2 = zeros(MAX_LENGTH,(DivM-1)*MAX_LENGTH),
                    Mat3 = zeros((DivN-1)*MAX_LENGTH,MAX_LENGTH),
                    Mat4 = zeros((DivN-1)*MAX_LENGTH,(DivM-1)*MAX_LENGTH),
                    recompose4(Mat1, Mat2, Mat3, Mat4);
                true ->
                    %(26x25)
                    Mat1 = zeros(RestN,MAX_LENGTH),
                    Mat2 = zeros(RestN,(DivM-1)*MAX_LENGTH),
                    Mat3 = zeros((DivN)*MAX_LENGTH,MAX_LENGTH),
                    Mat4 = zeros((DivN)*MAX_LENGTH,(DivM-1)*MAX_LENGTH),
                    recompose4(Mat1, Mat2, Mat3, Mat4)
                end;
            true ->
                if RestN == 0 ->
                    %(25x26)
                    Mat1 = zeros(MAX_LENGTH,RestM),
                    Mat2 = zeros(MAX_LENGTH, (DivM)*MAX_LENGTH),
                    Mat3 = zeros((DivN-1)*MAX_LENGTH,RestM),
                    Mat4 = zeros((DivN-1)*MAX_LENGTH,(DivM)*MAX_LENGTH),
                    recompose4(Mat1, Mat2, Mat3, Mat4);
                true ->
                    %(26x26)
                    Mat1 = zeros(RestN,RestM),
                    Mat2 = zeros(RestN,(DivM)*MAX_LENGTH),
                    Mat3 = zeros((DivN)*MAX_LENGTH,RestM),
                    Mat4 = zeros((DivN)*MAX_LENGTH,(DivM)*MAX_LENGTH),
                    recompose4(Mat1, Mat2, Mat3, Mat4)
                end
            end
        end
    end.

zeros_conc(N,M) ->
    zeros_conc(N, M, {self(), res}),
    receive 
        {res, Result} ->
            Result
    end.

zeros_conc(N, M, {Pid, ID}) ->
    MAX_LENGTH = get(max_length),

    if N =< MAX_LENGTH, M =< MAX_LENGTH ->
        Result = [[numerl:zeros(N,M)]],
        Pid ! {ID, Result};

    N =< MAX_LENGTH, M > MAX_LENGTH ->
        RestM = M rem MAX_LENGTH,
        DivM = M div MAX_LENGTH,
        if(RestM ==0 ) ->
            Append = spawn(block_mat,appendEach,[{self(), result}]),
            spawn(block_mat,zeros_conc,[N,MAX_LENGTH,{Append,a}]), 
            spawn(block_mat,zeros_conc,[N, (DivM -1)* MAX_LENGTH, {Append,b}]),
            receive 
                {result, Result} ->
                    Pid ! {ID,Result}
            end;
        true ->
            %(3x26)
            Append = spawn(block_mat,appendEach,[{self(),result}]),
            spawn(block_mat,zeros_conc,[N,RestM,{Append,a}]),
            spawn(block_mat,zeros_conc,[N, DivM* MAX_LENGTH,{Append,b}]),
            receive 
                {result, Result} ->
                    Pid ! {ID,Result}
            end
        end;

    N > MAX_LENGTH, M =< MAX_LENGTH ->
        RestN = N rem MAX_LENGTH,
        DivN = N div (MAX_LENGTH),
        if RestN == 0 ->
            %(25, 3)
            Append = spawn(block_mat,append,[{self(), result}]),
            spawn(block_mat,zeros_conc,[MAX_LENGTH, M, {Append,a}]), 
            spawn(block_mat,zeros_conc,[(DivN-1)*MAX_LENGTH, M, {Append,b}]),
            receive 
                {result, Result} ->
                    Pid ! {ID,Result}
            end;
            
        true ->
            %(26, 3)
            Append = spawn(block_mat,append,[{self(),result}]),
            spawn(block_mat,zeros_conc,[RestN, M,{Append, a}]), 
            spawn(block_mat,zeros_conc,[DivN*MAX_LENGTH, M, {Append,b}]),
            receive 
                {result, Result} ->
                    Pid ! {ID,Result}
            end
        end;

    N > MAX_LENGTH, M > MAX_LENGTH ->
        RestN = N rem MAX_LENGTH,
        DivN = N div (MAX_LENGTH),
        RestM = M rem MAX_LENGTH,
        DivM = M div MAX_LENGTH,

        if RestM == 0 ->
            if RestN == 0 ->
                %(25, 25)
                Recompose = spawn(block_mat,recompose4,[{self(),result}]),
                spawn(block_mat,zeros_conc,[MAX_LENGTH, MAX_LENGTH,{Recompose, a}]),
                spawn(block_mat,zeros_conc,[MAX_LENGTH, (DivM-1)*MAX_LENGTH,{Recompose, b}]),
                spawn(block_mat,zeros_conc,[(DivN-1)*MAX_LENGTH, MAX_LENGTH,{Recompose, c}]),
                spawn(block_mat,zeros_conc,[(DivN-1)*MAX_LENGTH, (DivM-1)*MAX_LENGTH,{Recompose, d}]),
                receive 
                {result, Result} ->
                    Pid ! {ID,Result}
                end;
            true ->
                %(26, 25)
                Recompose = spawn(block_mat,recompose4,[{self(),result}]),
                spawn(block_mat,zeros_conc,[RestN, MAX_LENGTH,{Recompose, a}]),
                spawn(block_mat,zeros_conc,[RestN, (DivM-1)*MAX_LENGTH,{Recompose, b}]),
                spawn(block_mat,zeros_conc,[(DivN)*MAX_LENGTH, MAX_LENGTH,{Recompose, c}]),
                spawn(block_mat,zeros_conc,[(DivN)*MAX_LENGTH, (DivM-1)*MAX_LENGTH,{Recompose, d}]),
                receive 
                {result, Result} ->
                    Pid ! {ID,Result}
                end
            end;
        true ->
            if RestN == 0 ->
                %(25, 26)
                Recompose = spawn(block_mat,recompose4,[{self(),result}]),
                spawn(block_mat,zeros_conc,[MAX_LENGTH, RestM,{Recompose, a}]),
                spawn(block_mat,zeros_conc,[MAX_LENGTH, (DivM)*MAX_LENGTH,{Recompose, b}]),
                spawn(block_mat,zeros_conc,[(DivN-1)*MAX_LENGTH, RestM,{Recompose, c}]),
                spawn(block_mat,zeros_conc,[(DivN-1)*MAX_LENGTH, (DivM)*MAX_LENGTH,{Recompose, d}]),
                receive 
                {result, Result} ->
                    Pid ! {ID,Result}
                end;
            true ->
                %(26, 26)
                Recompose = spawn(block_mat,recompose4,[{self(),result}]),
                spawn(block_mat,zeros_conc,[RestN, RestM,{Recompose, a}]),
                spawn(block_mat,zeros_conc,[RestN, (DivM)*MAX_LENGTH,{Recompose, b}]),
                spawn(block_mat,zeros_conc,[(DivN)*MAX_LENGTH, RestM,{Recompose, c}]),
                spawn(block_mat,zeros_conc,[(DivN)*MAX_LENGTH, (DivM)*MAX_LENGTH,{Recompose, d}]),
                receive 
                {result, Result} ->
                    Pid ! {ID,Result}
                end
            end
        end
    end.


-spec add(M1, M2) -> M3 when
    M1 :: matrix(),
    M2 :: matrix(),
    M3 :: matrix().

add(M1, M2) ->
    %N = length(M1),
    %M = length(lists:nth(1,M1)),
    element_wise_op(fun numerl:add/2, M1, M2).   

sub(M1, M2) ->
    element_wise_op(fun numerl:sub/2, M1, M2).

mult(M1, M2) ->
    multT(M1, tr(M2)).

tr(M) ->
    tr(M, []).

%% transpose matrix with accumulator
tr([[]|_], Rows) ->
    lists:reverse(Rows);
tr(M, Rows) ->
    {Row, Cols} = tr(M, [], []),
    tr(Cols, [Row|Rows]).


%% transpose the first row of a matrix with accumulators
tr([], Col, Cols) ->
    {lists:reverse(Col), lists:reverse(Cols)};
tr([[H|T]|Rows], Col, Cols) ->
    tr(Rows, [H|Col], [T|Cols]).

multT(M1, M2) ->
    [[lineSum(lists:zipwith(fun(A, B) ->
        numerl:dot(A, B) end, Li, Cj))
        || Cj <- M2]
        || Li <- M1].

lineSum([H|T]) ->
    lineSum(T, H).

lineSum(List, Acc) ->
    case List of [H|T] ->
        lineSum(T, numerl:add(Acc, H));
    [] ->
        Acc
    end.

scal(Const, M) ->
    lists:map(fun(Row) -> lists:map(fun(Elem)-> numerl:mult(Elem, Const) end, Row) end,M).

inv(M1) ->
    Len = length(M1),
    if Len > 1 ->
        {A,B,C,D} = split4(M1),
        InvA = inv(A),
        InvAB = mult(InvA,B),
        CInvA = mult(C,InvA),
        C4 = inv(sub(D,mult(C,InvAB))), % inv(D-C InvA B)
        Big = mult(C4,CInvA),
        C3 = scal(-1,Big),
        C2 = scal(-1,mult(InvAB,C4)),
        C1 = add(InvA,mult(InvAB,Big)),
        recompose4(C1,C2,C3,C4);
    true ->
        [[Bin]] = M1,
        [[numerl:inv(Bin)]]
    end.


splitLine(M1,Acc1,Acc2) -> 
    case M1 of 
        [] ->
            {lists:reverse(Acc1),lists:reverse(Acc2)};
        [H|T] ->
            {A1,A2} = lists:split(trunc(length(H)/2), H),
            splitLine(T, [A1|Acc1], [A2|Acc2])
        end.

splitLine(M1,Acc1,Acc2, N) -> 
    case M1 of 
        [] ->
            {lists:reverse(Acc1),lists:reverse(Acc2)};
        [H|T] ->
            {A1,A2} = lists:split(N, H),
            splitLine(T, [A1|Acc1], [A2|Acc2], N)
        end.

split4(M1) ->
    {A1,A2} = splitLine(M1,[],[]),
    {Xa, Xc} = lists:split(trunc(length(A1)/2), A1),
    {Xb, Xd} = lists:split(trunc(length(A2)/2), A2),
    {Xa,Xb,Xc,Xd}.

split4(M1, N, M) ->
    {A1,A2} = splitLine(M1,[],[], M),
    {Xa, Xc} = lists:split(N, A1),
    {Xb, Xd} = lists:split(N, A2),
    {Xa,Xb,Xc,Xd}.

recompose4(M1,M2,M3,M4) ->
    lists:append(appendEach(M1,M2),appendEach(M3,M4)).

recompose4({Pid,ID}) ->
    receive
        {a, A} ->
            receive
                {b, B} ->
                    receive
                        {c, C} ->
                            receive
                                {d, D} ->
                                    Result = recompose4(A,B,C,D),
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
                    Result = appendEach(A,B),
                    %erlang:display({sent, Pid, ID, dims(Result)}),
                    Pid ! {ID, Result}
            end
    end.

append({Pid, ID}) ->
    receive
        {a, A} ->
            receive
                {b, B} ->
                    Result = lists:append(A,B),
                    %erlang:display({sent, Pid, ID, dims(Result)}),
                    Pid ! {ID, Result}
            end
    end.

appendEach(M1,M2)-> 
    case {M1,M2} of 
        {[],[]} -> [];
        {[H1|T1],[H2|T2]} ->
            [lists:append(H1,H2)|appendEach(T1,T2)]
    end.

appendEachList(L) ->
    case L of
        [H|[]] -> H;
        [H|T] -> appendEach(H,appendEachList(T))
    end.

appendList(L) ->
    case L of
        [H|[]] ->H;
        [H|T] -> lists:append(H, appendList(T))
    end.

toErl(M) ->
    appendList(lists:map(fun(Row) -> appendEachList(lists:map(fun(Elem)-> numerl:mtfl(Elem) end, Row)) end,M)).

generateRandMat(0,_) ->
    [];
generateRandMat(Dim1,Dim2) ->
    [generateRandVect(Dim2)|generateRandMat(Dim1-1,Dim2)].
        
generateRandVect(0) ->
    [];
generateRandVect(Dim2) ->
    [rand:uniform(5)|generateRandVect(Dim2-1)].

benchmark() ->
    put(max_length,5).

first_try_benchmark() ->
    First_max = lists:min([round_one_benchmark(5) || _ <-lists:seq(1,5)]),
    erlang:display({first_max, First_max}),
    Second_max = lists:nth(2,lists:sort([round_two_benchmark(First_max) || _ <-lists:seq(1,20)])),
    erlang:display({second_max, Second_max}),
    put(max_length,Second_max).

round_one_benchmark(N) ->
    Passed = test_time(N,false),
    if Passed ->
        round_one_benchmark(N*2);
    true ->
        N div 2
    end.

round_two_benchmark(N) ->
    Passed = test_time(N, false),
    if Passed ->
        round_two_benchmark(round(N*1.2));
    true ->
        round(N/1.2)
    end.

test_time(N,true) ->
    timer:sleep(100),
    M = generateRandMat(N,N),
    Mat = numerl:matrix(M),
    TimesValues = [timer:tc(numerl,dot,[Mat,Mat]) || _ <- lists:seq(1,50)],
    Times = [Time || {Time, _} <- TimesValues],
    Test = lists:search(fun(Time) -> Time>1000 end,Times),
    case Test of {value, Time} ->
        erlang:display({N,Time}),
        false;
    _ ->
        true
    end;

    test_time(N,false) ->
        M = generateRandMat(N,N),
        Mat = numerl:matrix(M),
        {Time,_} = timer:tc(numerl,dot,[Mat,Mat]),
        if Time <1000 ->
            true;
        true ->
            erlang:display({foire, N,Time}),
            false
        end.

dims(Mat) ->
    lists:map(fun(Row) -> lists:map(fun({matrix,N,M,_}) -> {N,M} end,Row) end,Mat).
