:- ['lab12', 'library'].

%task39
printArrayWithStep2([], _).
printArrayWithStep2([H|T], Index) :-
    0 is Index mod 2,
    NewIndex is Index + 1,
    write(H),write(', '),
    printArrayWithStep2(T, NewIndex),!.
printArrayWithStep2([_|T], Index) :-
    NewIndex is Index + 1,
    printArrayWithStep2(T, NewIndex),!.

printArrayParts(List) :-
    printArrayWithStep2(List,1),
    nl,
    printArrayWithStep2(List,0).

%task45
findSum([],_,_,R,R).
findSum([H|T],A,B,R,LR) :-
    H =< B,
    H >= A,
    NewLR is LR + H,
    findSum(T,A,B,R,NewLR).
findSum([_|T],A,B,R,LR) :- findSum(T,A,B,R,LR).
findSum(List,A,B,R) :- findSum(List,A,B,R,0).

%task51
findCount([],_,R,R).

findCount([H|T],V,R,LR) :-
    equalLists(H,V), 
    NewLR is LR + 1,
    findCount(T,V,R,NewLR),!;
    findCount(T,V,R,LR).

findCount([H|T],V,R,LR) :-
    not(is_list(H)),
    H is V, 
    NewLR is LR + 1,
    findCount(T,V,R,NewLR),!;
    findCount(T,V,R,LR).
findCount(List,V,R) :- findCount(List,V,R,0).

generateList2(_,[],R,R).
generateList2(List,[H|T],List2,LocalList) :-
    findCount(List,H,Count),
    pushBack(LocalList, Count, NewLocal),
    generateList2(List,T,List2,NewLocal),!.
generateList2(List, List1, List2) :- generateList2(List,List1,List2,[]).

calculateLists(List,L1,L2) :-
    generateUniqueList(List,L1),
    generateList2(List,L1,L2).

%task14
findValues :- 
    List = [_,_,_],
    contains(List,[belocurov,_]),
    contains(List,[rizov,_]),
    contains(List,[chernov,_]),
    contains(List,[_,riziy]),
    contains(List,[_,blondin]),
    contains(List,[_,brunet]),
    not(contains(List,[rizov,riziy])),
    not(contains(List,[chernov,brunet])),
    write(List),!.

%task15
findValues2 :-
    List = [_,_,_],
    contains(List,[_,_,white]),
    contains(List,[_,_,blue]),
    contains(List,[_,_,green]),
    contains(List,[_,white,_]),
    contains(List,[_,blue,_]),
    contains(List,[_,green,_]),
    contains(List,[valya,_,_]),
    contains(List,[anya,A,A]),
    contains(List,[natasha,green,_]),
    not(contains(List,[valya,B,B])),
    not(contains(List,[natasha,C,C])),
    not(contains(List,[valya,white,_])),
    not(contains(List,[valya,_,white])),
    write(List),!.

%task16
findValues3 :-
    List = [_,_,_],
    contains(List,[_,_,1]),
    contains(List,[_,_,2]),
    contains(List,[A,slesar,0]),
    contains(List,[B,svarshik,_]),
    contains(List,[C,tokar,E]),
    contains(List,[boris,_,_]),
    contains(List,[ivanov,_,_]),
    contains(List,[semenov,_,D]),
    D > E,
    write('слесарь: '),write(A),write(', '),
    write('сварщик: '),write(B),write(', '),
    write('токарь: '),write(C),nl,
    write(List),!.
    
%task17
betweenList(_,_,_, [_,_]) :- fail,!.
betweenList(Center,FstSide,SndSide, [FstSide,Center,SndSide|_]) :- !.
betweenList(Center,FstSide,SndSide, [SndSide,Center,FstSide|_]) :- !.
betweenList(Center,FstSide,SndSide, [_,H2,H3|T]) :-
    betweenList(Center, FstSide, SndSide, [H2,H3|T]),!.
    
neibhor(Value1,Value2, [Value1,Value2|_]).
neibhor(Value1,Value2, [Value2,Value1|_]).
neibhor(_,_, [_,_|_]) :- fail,!.
neibhor(Value1,Value2, [_,H2|T]) :- neibhor(Value1,Value2,[H2|T]),!.

findValues4 :-
    List = [_,_,_,_],
    contains(List,[butilka,_]),
    contains(List,[stakan,_]),
    contains(List,[kuvshin,_]),
    contains(List,[banka,_]),
    contains(List,[_,moloko]),
    contains(List,[_,limonade]),
    contains(List,[_,kvAss]),
    contains(List,[_,voda]),
    not(contains(List,[butilka,moloko])),
    not(contains(List,[butilka,voda])),
    not(contains(List,[banka,limonade])),
    not(contains(List,[banka,voda])),
    neibhor([stakan,_],[banka,_],List),
    neibhor([stakan,_],[_,moloko],List),
    betweenList([_,limonade],[_,kvAss],[kuvshin,_],List),
    write(List),!.

%task18
findValues5 :-
    List = [_,_,_,_],
    contains(List,[voronov,_]),
    contains(List,[pavlov,_]),
    contains(List,[levizkiy,_]),
    contains(List,[saharov,_]),
    contains(List,[_,dancer]),
    contains(List,[_,artist]),
    contains(List,[_,singer]),
    contains(List,[_,writer]),
    not(contains(List,[voronov,singer])),
    not(contains(List,[levizkiy,singer])),
    not(contains(List,[pavlov,writer])),
    not(contains(List,[saharov,writer])),
    not(contains(List,[voronov,writer])),
    not(contains(List,[pavlov,artist])),
    write(List),!.

%task19
findValues6 :-
    List = [_,_,_],
    contains(List, [_,_,_,1]),
    contains(List, [_,_,_,2]),
    contains(List, [_,_,_,3]),
    contains(List,[_,_,american,B]),
    contains(List,[_,_,isralian,_]),
    contains(List,[_,_,australian,_]),

    contains(List,[_,basketball,_,_]),
    contains(List,[_,tennis,_,D]),
    contains(List,[_,kriket,_,1]),

    contains(List,[maikl,basketball,_,A]),
    contains(List,[saimon,_,isralian,C]),
    contains(List,[richard,_,_,_]),
    not(
        contains(List,[maikl,_,isralian,_]);
        contains(List,[maikl,_,american,_])
    ),
    A < B,
    C < D,
    write(List),!.

%task20
findValues7 :-
    List = [_,_,_],
    contains(List,[petr,_]),
    contains(List,[roman,_]),
    contains(List,[sergey,_]),
    contains(List,[_,mathemathic]),
    contains(List,[_,chemistry]),
    contains(List,[_,physic]),
    (
        not(contains(List,[petr,mathemathic]));
        contains(List,[sergey,physic])
    ),
    (
        contains(List,[roman,physic]);
        contains(List,[petr,mathemathic])
    ),
    (
        contains(List,[sergey,mathemathic]);
        contains(List,[roman,physic])    
    ),
    write(List),!.