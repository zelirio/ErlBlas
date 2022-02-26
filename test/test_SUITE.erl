-module(test_SUITE).
-include_lib("stdlib/include/assert.hrl").
-export([numerl_test/0]).

numerl_test() ->
    M1 = numerl:matrix([[4,2],[2,2]]),
    M2 = numerl:matrix([[3,5],[5,4]]),
    erlang:display(numerl:mtfli(numerl:dot(M1,M2))).

add_test() ->
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


mult_test() ->
    M = rand:uniform(10),
    N = rand:uniform(10),
    P = rand:uniform(10),
    io:format("M : ~w, N : ~w, P : ~w~n",[M,N,P]),
    A = generateRandMat(M,N),
    B = generateRandMat(N,P),
    NumA = block_mat:matrix(A),
    NumB = block_mat:matrix(B),
    block_mat:display_mat(NumA),
    block_mat:display_mat(NumB),
    block_mat:display_mat(block_mat:mult(NumA,NumB)).
    %io:format("test passed ? : ~w~n", [numerl:dot(C,D)==block_mat:mult2(NumA,NumB)]).

testInv() ->
    N = rand:uniform(50),
    A = generateRandMat(N,N),
    R1 = numerl:inv(A),
    %io:format("~w~n",[R1]),
    R2 = block_mat:inv(A),
    %io:format("~w~n",[R2]),
    io:format("test passed ? : ~w~n", [numerl:'=='(R1,R2)]).

matrix_test() ->
    erlang:display("matrix test~n"),
    N = 10,
    M = 10,
    M1 = generateRandMat(N,M),
    A = block_mat:matrix(M1),
    %erlang:display(A).
    printDim(A).

zeros_test() ->
    %N = 5,
    erlang:display(io:format("A: ~w~n",[42])),
    N = rand:uniform(50),
    M = rand:uniform(50),
    erlang:display(io_lib:format("sdfsdf ~B", [12312])),
    A = block_mat:zeros(N,M),
    %erlang:display(A),
    %B = mat:zeros(3,3),
    io:format("A: ~w~n",[A]),
    printDim(A).
    %io:format("B: ~w~n",[B]),
    %io:format("test passed ? : ~w~n", [mat:'=='(A,B)]).

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
