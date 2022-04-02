-module(test_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include_lib("eunit/include/eunit.hrl").

tr_testi() ->
    Mat1 = block_mat:matrix(generateRandMat(7,8)),
    erlang:display(block_mat:toErl(Mat1)),
    erlang:display(block_mat:toErl(block_mat:transpose(Mat1))).

daxpy_test() ->
    Xb = generateRandMat(7,8),
    X = block_mat:matrix(Xb),
    XN = numerl:matrix(Xb),
    Yb = generateRandMat(7,8),
    YN = numerl:matrix(Yb),
    Y = block_mat:matrix(Yb),
    block_mat:daxpy(1.5, X, Y),
    numerl:daxpy(1.5, XN, YN),
    ?assertNot(mat:'=='(Yb, block_mat:toErl(Y))), % Y has changed
    ?assert(mat:'=='(numerl:mtfl(YN), block_mat:toErl(Y))). % Value is correct

dscal_test() ->
    Xb = generateRandMat(7,8),
    X = block_mat:matrix(Xb),
    XN = numerl:matrix(Xb),
    block_mat:dscal(1.5, X),
    numerl:dscal(1.5, XN),
    ?assertNot(mat:'=='(Xb, block_mat:toErl(X))), % Y has changed
    ?assert(mat:'=='(numerl:mtfl(XN), block_mat:toErl(X))). % Value is correct

dgemm_testi() ->
    Mat1 = block_mat:matrix(generateRandMat(2,2)),
    erlang:display(mat1),
    erlang:display(block_mat:toErl(Mat1)),
    Mat2 = block_mat:matrix(generateRandMat(2,2)),
    erlang:display(mat2),
    erlang:display(block_mat:toErl(Mat2)),
    Mat3 = block_mat:matrix(generateRandMat(2,2)),
    erlang:display(mat3),
    erlang:display(block_mat:toErl(Mat3)),
    erlang:display(mult),
    erlang:display(block_mat:toErl(block_mat:mult(Mat1,Mat2))),
    erlang:display(dgemm),
    erlang:display(block_mat:toErl(block_mat:dgemm(false,false,1.0,0.0,Mat1,Mat2,Mat3))),
    erlang:display(plusC),
    erlang:display(block_mat:toErl(block_mat:dgemm(false,false,1.0,1.0,Mat1,Mat2,Mat3))),
    erlang:display(transpA),
    erlang:display(block_mat:toErl(block_mat:dgemm(true,false,1.0,0.0,Mat1,Mat2,Mat3))),
    erlang:display(transpB),
    erlang:display(block_mat:toErl(block_mat:dgemm(false,true,1.0,0.0,Mat1,Mat2,Mat3))),
    erlang:display(halfAB),
    erlang:display(block_mat:toErl(block_mat:dgemm(false,false,0.5,0.0,Mat1,Mat2,Mat3))),
    erlang:display(halfC),
    erlang:display(block_mat:toErl(block_mat:dgemm(false,false,1.0,0.5,Mat1,Mat2,Mat3))).

dims_testi() ->
    Mat = generateRandMat(12,16),
    Mat2 = block_mat:matrix(Mat),
    erlang:display(block_mat:dims(Mat2)).

oui_testi_() ->
    {timeout, 10,
        fun() ->
                zeros_conc_testi()
        end}.

zeros_conc_testi() ->
    N = 20000,
    M = 20000,
    erlang:display({dims,N,M}),
    block_mat:zeros(N,M),
    {NormalTime, Value} = timer:tc(block_mat,zeros,[N,M]),
    erlang:display({normal,NormalTime}),
    {ConcTime, Value} = timer:tc(block_mat,zeros_conc,[N,M]),
    erlang:display({conc,ConcTime}),
    erlang:display(block_mat:dims(Value)).

conc_testi()->
    N = rand:uniform(50),
    M = rand:uniform(50),
    erlang:display({dims,N,M}),
    Mat = generateRandMat(1000,1000),
    block_mat:matrix(Mat),
    block_mat:matrix_conc(Mat),
    {NormalTime, Value} = timer:tc(block_mat,matrix,[Mat]),
    erlang:display({normal,NormalTime}),
    {ConcTime, Value} = timer:tc(block_mat,matrix_conc,[Mat]),
    erlang:display({conc,ConcTime}).
    %erlang:display(erlang:system_info(logical_processors_available)).

benchmark_testi() ->
    block_mat:first_try_benchmark(),
    erlang:display(get(max_length)).
    %erlang:display(block_mat:test_time(get(max_length),true)),
    %erlang:display(block_mat:test_time(get(max_length),true)),
    %erlang:display(block_mat:test_time(get(max_length),true)),
    %erlang:display(block_mat:test_time(get(max_length),true)).

max_length_testi() ->
    M1 = generateRandMat(7,4),
    erlang:display(M1),
    M2 = block_mat:matrix(M1),
    M1 = block_mat:toErl(M2),
    erlang:display(block_mat:toErl(block_mat:zeros(7,4))),
    erlang:display(block_mat:toErl(block_mat:eye(7))),
    erlang:display(block_mat:toErl(block_mat:matrix(M1))).

numerl_testi() ->
    M1 = numerl:matrix([[4,2],[2,2]]),
    M2 = numerl:matrix([[3,5],[5,4]]),
    erlang:display(numerl:mtfli(numerl:dot(M1,M2))).

eye_testi()->
    N = rand:uniform(25),
    erlang:display(N),
    Mat = block_mat:eye(N),
    block_mat:display_mat(Mat).

add_testi() ->
    N = 2,
    M = 2,
    M1 = generateRandMat(N,M),
    M2 = generateRandMat(N,M),
    %A = block_mat:matrix(M1),
    %B = block_mat:matrix(M2),
    %C = block_mat:mult2(A, B),
    A2 = numerl:matrix(M1),
    B2 = numerl:matrix(M2),
    D = numerl:dot(A2, B2),
    %[[A2]] = A,
    %[[B2]] = B,
    %[[C2]] = C,
    %erlang:display(C).
    erlang:display(numerl:mtfl(A2)),
    erlang:display(numerl:mtfl(B2)),
    erlang:display(numerl:mtfl(D)).
    %erlang:display(numerl:mtfli(C2)).
    %printDim(A),
    %printDim(B),
    %printDim(C).

mult_testi() ->
    M = rand:uniform(10),
    N = rand:uniform(10),
    P = rand:uniform(10),
    io:format("M : ~w, N : ~w, P : ~w~n",[M,N,P]),
    A = generateRandMat(M,P),
    B = generateRandMat(P,N),
    NumA = block_mat:matrix(A),
    NumB = block_mat:matrix(B),
    erlang:display(lists:flatten(io_lib:format("~w",[block_mat:toErl(block_mat:mult(NumA,NumB))]))).
    %io:format("test passed ? : ~w~n", [numerl:dot(C,D)==block_mat:mult2(NumA,NumB)]).

testInv() ->
    N = rand:uniform(50),
    A = generateRandMat(N,N),
    R1 = numerl:inv(A),
    %io:format("~w~n",[R1]),
    R2 = block_mat:inv(A),
    %io:format("~w~n",[R2]),
    io:format("test passed ? : ~w~n", [numerl:'=='(R1,R2)]).

matrix_testi() ->
    erlang:display("matrix test~n"),
    N = 10,
    M = 10,
    M1 = generateRandMat(N,M),
    A = block_mat:matrix(M1),
    %erlang:display(A).
    printDim(A).

zeros_testi() ->
    %N = 5,
    N = rand:uniform(20),
    M = rand:uniform(20),
    erlang:display(io_lib:format("~w~w", [N,M])),
    A = block_mat:zeros(N,M),
    block_mat:display_mat(A).

printDim(A) ->
    case A of 
        [H|T] ->
            io:format("H: ~w~n",[H]),
            printDimLine(H),
            io:format("~n"),
            printDim(T);
        [] ->
            io:format("~n")
    end.

printDimLine(H) ->
    case H of
        [A|B] ->
            %io:format("A: ~w~n",[A]),
            %erlang:display(A),
            erlang:display(numerl:mtfli(A)),
            %io:format("(~w x ~w) ",[length(A),length(lists:nth(1,A))]),
            printDimLine(B);
        [] ->
            io:format("~n")
        end.


generateRandMat(0,_) ->
    [];
generateRandMat(Dim1,Dim2) ->
    [generateRandVect(Dim2)|generateRandMat(Dim1-1,Dim2)].
    

generateRandVect(0) ->
    [];

generateRandVect(Dim2) ->
    [rand:uniform(5)|generateRandVect(Dim2-1)].

poster_tes() ->
    M1 = generateRandMat(223,223),
    M2 = generateRandMat(223,223),
    MM1 = numerl:matrix(M1),
    MM2 = numerl:matrix(M2),
    A= os:timestamp(),
    %numerl:mult(MM1,MM2),
    block_mat:mult(M1,M2),
    B= os:timestamp(),
    erlang:display("ici"),
    erlang:display(timer:now_diff(B,A)/1000000),
    erlang:display("là").
