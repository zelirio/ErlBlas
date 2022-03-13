-module(utils).
-export([generateRandMat/2, append/1, appendEach/1, appendEach/2, appendEachList/1, appendList/1, splitLine/3, split4/1, recompose4/4, recompose4/1, split4/3, splitLine/4]).

generateRandMat(0,_) ->
    [];
generateRandMat(Dim1,Dim2) ->
    [generateRandVect(Dim2)|generateRandMat(Dim1-1,Dim2)].
    
generateRandVect(0) ->
    [];

generateRandVect(Dim2) ->
    [rand:uniform(5)|generateRandVect(Dim2-1)].

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
