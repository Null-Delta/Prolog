man(voeneg).
man(ratibor).
man(boguslav).
man(velerad).
man(duhovlad).
man(svyatoslav).
man(dobrozhir).
man(bogomil).
man(zlatomir).

woman(goluba).
woman(lubomila).
woman(bratislava).
woman(veslava).
woman(zhdana).
woman(bozhedara).
woman(broneslava).
woman(veselina).
woman(zdislava).

parent(voeneg,ratibor).
parent(voeneg,bratislava).
parent(voeneg,velerad).
parent(voeneg,zhdana).

parent(goluba,ratibor).
parent(goluba,bratislava).
parent(goluba,velerad).
parent(goluba,zhdana).

parent(ratibor,svyatoslav).
parent(ratibor,dobrozhir).
parent(lubomila,svyatoslav).
parent(lubomila,dobrozhir).

parent(boguslav,bogomil).
parent(boguslav,bozhedara).
parent(bratislava,bogomil).
parent(bratislava,bozhedara).

parent(velerad,broneslava).
parent(velerad,veselina).
parent(veslava,broneslava).
parent(veslava,veselina).

parent(duhovlad,zdislava).
parent(duhovlad,zlatomir).
parent(zhdana,zdislava).
parent(zhdana,zlatomir).

%task11
dauther(X,Y) :- parent(Y,X),woman(X).
dauther(X) :- parent(X,Y),woman(Y),print(Y),nl,fail.
%task12
wife(X,Y) :- parent(X,Z),parent(Y,Z),woman(X),man(Y).
wife(X) :- wife(Y,X),print(Y),nl.
%task13
grand_ma(X,Y) :- parent(X,Z),parent(Z,Y),woman(X).
grand_mas(X) :- parent(Y,X),parent(Z,Y),print(Z),nl,fail.
%task14
grand_ma_and_da(X,Y) :- woman(Y),woman(X),(grand_ma(X,Y);grand_ma(Y,X)).
%task15
findMinNumberUp(X,Y) :- X < 10, Y is X.
findMinNumberUp(X,Y) :- 
    N is X mod 10,
    V is X div 10,
    findMinNumberUp(V,C),
    Y is min(N,C).
%task16
findMinNumberDown(X,Y,Z) :- X < 10, Y is min(X,Z).
findMinNumberDown(X,Y,Z) :- 
    N is X mod 10,
    V is X // 10,
    NewZ is min(Z,N),
    findMinNumberDown(V,Y,NewZ).
findMinNumberDown(X,Y) :- findMinNumberDown(X,Y,10).
%task17
findMultNumsUp(X,Y) :- 
    X < 10,
    Mod is X mod 5,
    (0 is Mod, Y is 1; Y is X).
findMultNumsUp(X,Y) :-
    V is X div 10,
    N is X mod 10,
    Mod is N mod 5,
    (0 is Mod, findMultNumsUp(V,Y);(findMultNumsUp(V,C), Y is N * C)).
%task18
findMultNumsDown(X,Y,Z) :- 
    X < 10,
    Mod is X mod 5,
    (0 is Mod,Y is Z; Y is Z * X).
findMultNumsDown(X,Y,Z) :-
    N is X mod 10,
    V is X div 10,
    Mod is N mod 5,
    (0 is Mod,NewZ is Z;NewZ is N * Z),
    findMultNumsDown(V,Y,NewZ).

findMultNumsDown(X,Y) :- findMultNumsDown(X,Y,1).
%task19
fibUp(N,X) :- N < 3, X is 1.
fibUp(N, X) :- N1 is N - 1, N2 is N - 2, fibUp(N1,X1), fibUp(N2,X2), X is X1 + X2.
%task20
fibDown(0,X,LX,PLX) :- X is LX + PLX.
fibDown(N,X,LX,PLX) :- N < 0, X is 1.
fibDown(N,X,LX,PLX) :-
    NewN is N - 1,
    NLX is LX + PLX,
    NPLX is LX,
    fibDown(NewN, X, NLX,NPLX).
fibDown(N,X) :- N1 is N - 2, fibDown(N1,X,1,0).