:- ['library'].

isEasy(Value,Value).
isEasy(1,_).
isEasy(Value, Iter) :- 
    not(0 is Value mod Iter), 
    NewIter is Iter + 1,
    isEasy(Value, NewIter),!.
isEasy(X) :- isEasy(X,2).

%task11
findMaxEasyDivider(X,Y,Del) :-
    0 is X mod Del,
    isEasy(Del),
    Y is Del,!.
findMaxEasyDivider(X,Y,Del) :-
    NewDel is Del - 1,
    findMaxEasyDivider(X,Y,NewDel).
findMaxEasyDividerDown(X,Y) :- findMaxEasyDivider(X,Y,X).

%task12
multNums(X,Y) :- 
    X < 10, 
    Y is X.
multNums(X,Y) :- 
    V is X div 10,
    multNums(V, Y2),
    N is X mod 10,
    Y is Y2 * N.

nod(A,0,A) :- !.
nod(_,1,1) :- !.
nod(A,B,C) :- NewB is A mod B, nod(B, NewB, C).

findMaxUnEasyDivider(X,Y,Del) :-
    0 is X mod Del,
    not(0 is Del mod 2),
    not(isEasy(Del)),
    Y is Del,!.
findMaxUnEasyDivider(X,Y,Del) :-
    NewDel is Del - 1,
    findMaxUnEasyDivider(X,Y,NewDel).
findMaxUnEasyDivider(X,Y) :- findMaxUnEasyDivider(X,Y,X).

task12(X,Y) :- multNums(X,N1),findMaxUnEasyDivider(X,N2), nod(N1,N2,Y).

%task13
divBy2(X,R) :- 
    Mod is X mod 2,
    Val is X div 2,
    (0 is Mod,divBy2(Val,R);R is X).
divBy5(X,R) :- 
    Mod is X mod 5,
    Val is X div 5,
    (0 is Mod,divBy5(Val,R);R is X).
numForPeriod(X,R) :- divBy2(X,R1),divBy5(R1,R2),R is R2.

period(D,R) :- numForPeriod(D,Res),period(Res,R,1),!.
period(D,R,LR) :- 
    B10 is 10**LR,
    1 is B10 mod D,
    R is LR,!.
period(D,R,LR) :- 
    B10 is 10**LR,
    0 is B10 mod D,
    R is 0,!.
period(D,R,LR) :- 
    NewLR is LR + 1,
    period(D,R, NewLR).

findMaxPeriod(D) :- findMaxPeriod(D,2,0,2).
findMaxPeriod(D,1000,_,LocalIndex) :- D is LocalIndex,!.
findMaxPeriod(D,Index,LocalD, LocalIndex) :-
    period(Index,NextD),
    NewLocalD is max(NextD,LocalD),
    (NewLocalD>LocalD,NewLocalIndex is Index; NewLocalIndex is LocalIndex),
    NewIndex is Index + 1,
    findMaxPeriod(D,NewIndex, NewLocalD, NewLocalIndex),!.

%task14
lenght([],0).
lenght([_|Tail],X) :- lenght(Tail,V), X is V + 1.

%task15-3
findMax([],X,Z) :- X is Z.
findMax([H|T],X,Z) :- 
    NewZ is max(H,Z),
    findMax(T,X,NewZ).
findMax([H|T],X) :- findMax([H|T],X,H).

isMax(List, Index) :- 
    findMax(List,Max),
    getValue(List,Index,V),
    Max is V,!.

%task15-11
getNum(A,A,B,R) :- R is B.
getNum(A,B,A,R) :- R is B.
getNum(B,A,A,R) :- R is B.
isEquals(A,A,A).

getExcluzive([H|[H2|[H3|Tail]]], X) :- (isEquals(H,H2,H3),getExcluzive([H2,H3|Tail], X);getNum(H,H2,H3,X),!).

%task15-13
findMin([],X,Z) :- X is Z.
findMin([H|T],X,Z) :- 
    NewZ is min(H,Z),
    findMin(T,X,NewZ).
findMin([H|T],X) :- findMin([H|T],X,H).

replaceElements([],0,NewList,LocalList) :- NewList = LocalList.
replaceElements([H|T],0,NewList,LocalList) :-
    reverseList([H|T],[NH|NT]),
    reverseList(NT,NewInput),
    replaceElements(NewInput,0,NewList, [NH|LocalList]).
replaceElements([H|T], Index, NewList, LocalList) :-
    reverseList(LocalList,ReversedList),
    AddedList = [H|ReversedList],
    reverseList(AddedList, NewLocal),
    NewIndex is Index - 1,
    replaceElements(T, NewIndex, NewList, NewLocal).
replaceElements(List, Index, NewList) :- replaceElements(List, Index, NewList, []).

replaceBeforeMin(List, NewList) :-
    findMin(List,Min),
    findIndex(List,Min,Index),
    replaceElements(List, Index, NewList),!.

%task15-15
isLocalMin([H1,H2],1) :- H2 =< H1.
isLocalMin([H1,H2,H3|_],1) :- H2 =< H1, H2 =< H3.
isLocalMin([H1,H2 | _],0) :- H1 =< H2,!.
isLocalMin([_,H2,H3|T],Index) :-
    not(1 is Index),
    NewIndex is Index - 1,
    isLocalMin([H2,H3|T], NewIndex),!.

%task15-27
arrayLeftSwap([H|T], X) :-
    reverseList(T,T2),
    reverseList([H|T2],X).

%task15-30
isLocalMax([H1,H2],1) :- H2 >= H1.
isLocalMax([H1,H2,H3|_],1) :- H2 >= H1, H2 >= H3.
isLocalMax([H1,H2 | _],0) :- H1 >= H2,!.
isLocalMax([_,H2,H3|T],Index) :-
    not(1 is Index),
    NewIndex is Index - 1,
    isLocalMax([H2,H3|T], NewIndex),!.