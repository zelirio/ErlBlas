-module(block_mat).

-export([add/2, sub/2, mult2/2, inv/1, zeros/2, matrix/1]).

-define(MAX_LENGTH, 5).

-type matrix() :: [[number(), ...], ...].

%eye(N) ->
%    RestN = N rem ?MAX_LENGTH,
%   DivN = N div (?MAX_LENGTH + 1),

%   ok.

element_wise_op(Op, M1, M2) ->
    lists:zipwith(fun(L1, L2) -> lists:zipwith(Op, L1, L2) end, M1, M2).

matrix(Mat) ->
    erlang:display(Mat),
    N = length(Mat),
    M = length(lists:nth(1,Mat)),
    matrix(Mat, N, M).

matrix(Mat, N, M) when N =< ?MAX_LENGTH, M =< ?MAX_LENGTH ->
    [[numerl:matrix(Mat)]];

matrix(Mat, N, M) when N =< ?MAX_LENGTH, M > ?MAX_LENGTH ->
    RestM = M rem ?MAX_LENGTH,
    DivM = M div ?MAX_LENGTH,
    if(RestM ==0 ) ->
        {L, R} = splitLine(Mat,[],[], ?MAX_LENGTH),
        A = matrix(L, N,?MAX_LENGTH), 
        B = matrix(R, N, (DivM -1)* ?MAX_LENGTH),
        appendEach(A, B);
    true ->
        %(3x26)
        {L, R} = splitLine(Mat,[],[], RestM),
        A = matrix(L, N,RestM),
        B = matrix(R, N, DivM* ?MAX_LENGTH),
        appendEach(A, B)
    end;


matrix(Mat, N, M) when N > ?MAX_LENGTH, M =< ?MAX_LENGTH ->
    RestN = N rem ?MAX_LENGTH,
    DivN = N div (?MAX_LENGTH),
    if RestN == 0 ->
        %(25, 3)
        {M1, M2} = lists:split(?MAX_LENGTH, Mat),
        A = matrix(M1, ?MAX_LENGTH, M), 
        B = matrix(M2, (DivN-1)*?MAX_LENGTH, M),
        lists:append(A,B);
    true ->
        %(26, 3)
        {M1, M2} = lists:split(RestN, Mat),
        A = matrix(M1, RestN, M), 
        B = matrix(M2, DivN*?MAX_LENGTH, M),
        lists:append(A,B)
    end;


matrix(Mat, N, M) when N > ?MAX_LENGTH, M > ?MAX_LENGTH ->
    RestN = N rem ?MAX_LENGTH,
    DivN = N div (?MAX_LENGTH),
    RestM = M rem ?MAX_LENGTH,
    DivM = M div ?MAX_LENGTH,

    if RestM == 0 ->
        if RestN == 0 ->
            %(25, 25)
            {A, B, C, D} = split4(Mat, ?MAX_LENGTH, ?MAX_LENGTH),
            Mat1 = matrix(A, ?MAX_LENGTH, ?MAX_LENGTH),
            Mat2 = matrix(B, ?MAX_LENGTH, (DivM-1)*?MAX_LENGTH),
            Mat3 = matrix(C, (DivN-1)*?MAX_LENGTH, ?MAX_LENGTH),
            Mat4 = matrix(D, (DivN-1)*?MAX_LENGTH, (DivM-1)*?MAX_LENGTH),
            recompose4(Mat1, Mat2, Mat3, Mat4);
        true ->
            %(26, 25)
            {A, B, C, D} = split4(Mat, RestN, ?MAX_LENGTH),
            Mat1 = matrix(A, RestN, ?MAX_LENGTH),
            Mat2 = matrix(B, RestN, (DivM-1)*?MAX_LENGTH),
            Mat3 = matrix(C, (DivN)*?MAX_LENGTH, ?MAX_LENGTH),
            Mat4 = matrix(D, (DivN)*?MAX_LENGTH, (DivM-1)*?MAX_LENGTH),
            recompose4(Mat1, Mat2, Mat3, Mat4)
        end;
    true ->
        if RestN == 0 ->
            %(25, 26)
            {A, B, C, D} = split4(Mat, ?MAX_LENGTH, RestM),
            Mat1 = matrix(A, ?MAX_LENGTH, RestM),
            Mat2 = matrix(B, ?MAX_LENGTH, (DivM)*?MAX_LENGTH),
            Mat3 = matrix(C, (DivN-1)*?MAX_LENGTH, RestM),
            Mat4 = matrix(D, (DivN-1)*?MAX_LENGTH, (DivM)*?MAX_LENGTH),
            recompose4(Mat1, Mat2, Mat3, Mat4);
        true ->
            %(26, 26)
            {A, B, C, D} = split4(Mat, RestN, RestM),
            Mat1 = matrix(A, RestN, RestM),
            Mat2 = matrix(B, RestN, (DivM)*?MAX_LENGTH),
            Mat3 = matrix(C, (DivN)*?MAX_LENGTH, RestM),
            Mat4 = matrix(D, (DivN)*?MAX_LENGTH, (DivM)*?MAX_LENGTH),
            recompose4(Mat1, Mat2, Mat3, Mat4)
        end
    end.
    
    

zeros(N,M) ->
    RestN = N rem ?MAX_LENGTH,
    DivN = N div (?MAX_LENGTH + 1),
    Div2N = N div (?MAX_LENGTH),

    RestM = M rem ?MAX_LENGTH,
    DivM = M div (?MAX_LENGTH + 1),
    Div2M = M div (?MAX_LENGTH),
    
    if DivN == 0 ->
        % CORRECT
        if DivM == 0 ->
            [[numerl:zeros(N,M)]];
        true ->
            if RestM == 0 ->
                %(3x25) 
                A = zeros(N,?MAX_LENGTH), 
                B = zeros(N,(Div2M-1)*?MAX_LENGTH), 
                appendEach(A,B);
            true ->
                %(3x26)
                A = zeros(N,RestM),
                B = zeros(N,(Div2M)*?MAX_LENGTH),
                appendEach(A,B)
            end
        end;
    true ->
        if DivM == 0 ->
            if RestN == 0 ->
                %(25, 3)
                A = zeros(?MAX_LENGTH, M), 
                B = zeros((Div2N-1)*?MAX_LENGTH,M),
                lists:append(A,B);
            true ->
                %(26, 3)
                A = zeros(RestN, M), 
                B = zeros((Div2N)*?MAX_LENGTH,M),
                lists:append(A,B)
            end;
        true ->
            if RestM == 0 ->
                if RestN == 0 ->
                    %(25, 25)
                    Mat1 = zeros(?MAX_LENGTH,?MAX_LENGTH),
                    Mat2 = zeros(?MAX_LENGTH,(Div2M-1)*?MAX_LENGTH),
                    Mat3 = zeros((Div2N-1)*?MAX_LENGTH,?MAX_LENGTH),
                    Mat4 = zeros((Div2N-1)*?MAX_LENGTH,(Div2M-1)*?MAX_LENGTH),
                    recompose4(Mat1, Mat2, Mat3, Mat4);
                true ->
                    %(26, 25)
                    Mat1 = zeros(RestN,?MAX_LENGTH),
                    Mat2 = zeros(RestN,(Div2M-1)*?MAX_LENGTH),
                    Mat3 = zeros((Div2N)*?MAX_LENGTH,?MAX_LENGTH),
                    Mat4 = zeros((Div2N)*?MAX_LENGTH,(Div2M-1)*?MAX_LENGTH),
                    recompose4(Mat1, Mat2, Mat3, Mat4)
                end;
            true ->
                if RestN == 0 ->
                    %(25, 26)
                    Mat1 = zeros(?MAX_LENGTH,RestM),
                    Mat2 = zeros(?MAX_LENGTH, (Div2M)*?MAX_LENGTH),
                    Mat3 = zeros((Div2N-1)*?MAX_LENGTH,RestM),
                    Mat4 = zeros((Div2N-1)*?MAX_LENGTH,(Div2M)*?MAX_LENGTH),
                    recompose4(Mat1, Mat2, Mat3, Mat4);
                true ->
                    %(26, 26)
                    Mat1 = zeros(RestN,RestM),
                    Mat2 = zeros(RestN,(Div2M)*?MAX_LENGTH),
                    Mat3 = zeros((Div2N)*?MAX_LENGTH,RestM),
                    Mat4 = zeros((Div2N)*?MAX_LENGTH,(Div2M)*?MAX_LENGTH),
                    recompose4(Mat1, Mat2, Mat3, Mat4)
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

mult2(M1, M2) ->
    mult2T(M1, tr(M2)).

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

mult2T(M1, M2) ->
    %erlang:display(M1),
    %erlang:display(M2),
    [[lineSum(lists:zipwith(fun(Li, Cj) -> erlang:display(numerl:mtfli(Li)), erlang:display(numerl:mtfli(Cj)), numerl:dot(Li, Cj) end, Li, Cj))
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

mult(M1,M2)->
    Dim1 = [length(M1)|length(lists:nth(1,M1))],
    Dim2 = [length(M2)|length(lists:nth(1,M2))],
    mult(M1,M2,Dim1,Dim2).

mult(M1,M2,[M|N],[N|P]) when N > ?MAX_LENGTH, M> ?MAX_LENGTH, P> ?MAX_LENGTH -> %done
    CN = trunc(N/2),
    CM = trunc(M/2),
    CP = P-CM,
    if CP<1 ->
        CN = trunc(N/2),
        CM = trunc(M/2),
        {A13, A24} = splitLine(M1,[],[],CN),
        {A1,A3} = lists:split(CM,A13),
        {A2,A4} = lists:split(CM,A24),
        {B1,B2} = lists:split(CN,M2),
        C1 = add(mult(A1,B1),mult(A2,B2)),
        C2 = add(mult(A3,B1),mult(A4,B2)),
        lists:append(C1,C2);
    true ->
        {A13, A24} = splitLine(M1,[],[],CN),
        {A1,A3} = lists:split(CM,A13),
        {A2,A4} = lists:split(CM,A24),
        {B13, B24} = splitLine(M2,[],[],CP),
        {B1,B3} = lists:split(CN,B13),
        {B2,B4} = lists:split(CN,B24),
        C1 = add(mult(A1,B1),mult(A2,B3)),
        C2 = add(mult(A1,B2),mult(A2,B4)),
        C3 = add(mult(A3,B1),mult(A4,B3)),
        C4 = add(mult(A3,B2),mult(A4,B4)),
        recompose4(C1,C2,C3,C4)
    end;
    
mult(M1,M2,[_|N],[N|P]) when N > ?MAX_LENGTH, P> ?MAX_LENGTH -> %done
    CN = trunc(N/2),
    CP = trunc(P/2),
    {A1,A2} = splitLine(M1,[],[], CN),
    {B1,B2} = lists:split(CN,M2),
    {B11,B12} = splitLine(B1,[],[], CP),
    {B21,B22} = splitLine(B2,[],[], CP),
    C1 = add(mult(A1,B11),mult(A2,B21)),
    C2 = add(mult(A1,B12), mult(A2,B22)),
    appendEach(C1,C2);

mult(M1,M2,[M|N],[N|_]) when N > ?MAX_LENGTH, M> ?MAX_LENGTH-> %done
    CN = trunc(N/2),
    CM = trunc(M/2),
    {A13, A24} = splitLine(M1,[],[],CN),
    {A1,A3} = lists:split(CM,A13),
    {A2,A4} = lists:split(CM,A24),
    {B1,B2} = lists:split(CN,M2),
    C1 = add(mult(A1,B1),mult(A2,B2)),
    C2 = add(mult(A3,B1),mult(A4,B2)),
    lists:append(C1,C2);

mult(M1,M2,[_|N],[N|_]) when N > ?MAX_LENGTH -> % done
    CN = trunc(N/2),
    {A1,A2} = splitLine(M1,[],[],CN),
    {B1,B2} = lists:split(CN, M2),
    C1 = mult(A1,B1),
    C2 = mult(A2,B2),
    add(C1,C2);

mult(M1,M2,[M|N],[N|P]) when M> ?MAX_LENGTH, P> ?MAX_LENGTH -> % done
    CM = trunc(M/2),
    CP = P-CM,
    if CP<1 ->
        Co1 = trunc(M/2),
        {A1,A2} = lists:split(Co1, M1),
        C1 = mult(A1,M2),
        C2 = mult(A2,M2),
        lists:append(C1,C2);
    true ->
        {A1,A2} = lists:split(CM,M1),
        {B1,B2} = splitLine(M2,[],[],CP),
        recompose4(mult(A1,B1),mult(A1,B2),mult(A2,B1),mult(A2,B2))
    end;

mult(M1,M2,[_|N],[N|P]) when P> ?MAX_LENGTH -> % done
    CP = trunc(P/2),
    {B1,B2} = splitLine(M2,[],[],CP),
    C1 = mult(M1,B1),
    C2 = mult(M1,B2),
    appendEach(C1,C2);

mult(M1,M2,[M|N],[N|_]) when M> ?MAX_LENGTH-> % done
    Co1 = trunc(M/2),
    {A1,A2} = lists:split(Co1, M1),
    C1 = mult(A1,M2),
    C2 = mult(A2,M2),
    lists:append(C1,C2);

mult(M1,M2,[_|N],[N|_]) -> %done
    mat:'*'(M1,M2).

inv(M1) ->
    Len = length(M1),
    if Len > ?MAX_LENGTH ->
        {A,B,C,D} = split4(M1),
        InvA = inv(A),
        InvAB = mult(InvA,B),
        CInvA = mult(C,InvA),
        C4 = inv(sub(D,mult(C,InvAB))), % inv(D-C InvA B)
        Big = mult(C4,CInvA),
        C3 = numerl:dot(-1,Big), % need to change to use ours
        C2 = numerl:dot(-1,mult(InvAB,C4)), % need to change to use ours
        C1 = add(InvA,mult(InvAB,Big)),
        recompose4(C1,C2,C3,C4);
    true ->
        numerl:inv(M1)
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

appendEach(M1,M2)-> 
    case {M1,M2} of 
        {[],[]} -> [];
        {[H1|T1],[H2|T2]} ->
            [lists:append(H1,H2)|appendEach(T1,T2)]
    end.

        
    