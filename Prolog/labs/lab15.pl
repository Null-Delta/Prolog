:- ['library','lab14','combinatorics', 'graphs'].

%task1
lab15task1 :- writeResultInFile(generateLab15Task1List(_), 'lab15task1output.txt').
    
generateLab15Task1List(List) :-
    Chars = [97,98,99,100,101,102],
    List ~ [_,_,_,_,_,_,_],
    Chars --/ A /-- CWA,
    CWA --/ B /-- CWAB,
    CWAB /?- 2 --/ [C,D],
    List <-- A -? 3,
    List <-- B -? 2,
    List <-- [C,D] -? 1,
    writeString(List),nl.

%task2
lab15task2 :- writeResultInFile(generateLab15Task2List(_), 'lab15task2output.txt').

generateLab15Task2List(List) :-
    Chars = [97,98,99,100,101,102],
    List ~ [_,_,_,_,_,_,_,_,_],
    Chars /?- 2 --/ [A,B] /-- CWAB,
    CWAB --/ C /-- CWABC,
    CWABC /?- 2 --/ [D,E],
    List <-- C -? 3,
    List <-- [A,B] -? 2,
    List <-- [D,E] -? 1,
    writeString(List),nl.

%task3
lab15task3 :- writeResultInFile(generateLab15Task3List(_), 'lab15task3output.txt').

generateLab15Task3List(List) :-
    Chars = [97,98,99,100,101,102],
    List ~ [_,_,_,_],
    Chars --/ 97 /-- CWA,
    CWA --/ B,
    List <-- 97 -? 3, List <-- B,
    writeString(List),nl.

generateLab15Task3List(List) :-
    List ~ [_,_,_,_],
    List <-- 97 -? 4,
    writeString(List),nl.

%task4
lab15task4 :- writeResultInFile(generateLab15Task4List(_), 'lab15task4output.txt').

generateLab15Task4List(List) :-
    Chars = [97,98,99,100,101,102],
    List ~ [_,_,_,_,_,_,_],
    Chars --/ A /-- CWA,
    CWA /?- 3 --/ [B,C,D],
    List <-- [B,C,D] -? 1,
    List <-- A -? 4,
    writeString(List),nl.

generateLab15Task4List(List) :-
    Chars = [97,98,99,100,101,102],
    List ~ [_,_,_,_,_,_,_],
    Chars --/ A /-- CWA,
    CWA --/ B /-- CWAB,
    CWAB /?- 2 --/ [C,D],
    List <-- A -? 3,
    List <-- B -? 2,
    List <-- [C,D] -? 1,
    writeString(List),nl.

generateLab15Task4List(List) :-
    Chars = [97,98,99,100,101,102],
    List ~ [_,_,_,_,_,_,_],
    Chars --/ A /-- CWA,
    CWA /?- 3 --/ [B,C,D],
    List <-- A,
    List <-- [B,C,D] -? 2,
    writeString(List),nl.

%task5
lab15task5(N,K) :-
    (exists_file('lab15task5output.txt'),delete_file('lab15task5output.txt'); told ),
    tell('lab15task5output.txt'),
    findall(_, generateLab15Task5List(N,K), _),
    told,!.

generateLab15Task5List(N,K) :-
    Chars = [97,98,99,100,101,102],
    length(Line,N),
    Chars /?- 2 --/ [A,B] /-- CWAB,
    CWAB --/ C /-- CWABC,
    NewN is N - K - 4,
    length(LastChars, NewN),
    CWABC /?- NewN --/ LastChars,
    Line <-- [A,B] -? 2,
    Line <-- C -? K,
    Line <-- LastChars -? 1,
    writeString(Line),nl.

%task6
generateOstovTree((V,E),Result) :-
    E <= SubE,
    is_constrain((V,SubE)),
    is_tree((V,SubE)),
    Result ~ (V,SubE).

countOfOstovs :-
    readGraph(Graph, 'lab15_graph_input.txt'),
    findall(Ostov, generateOstovTree(Graph, Ostov),Ostovs),
    length(Ostovs,X),
    write(X).

%task7
getGameltonWay(Way) :-
    readGraph((V,E), 'lab15_graph_input2.txt'),
    V <- X,
    write(X),
    (V,E) : X ~~ Y ~> Way,
    V @: (
        [Vertex]>>(
            Way <- Vertex
        )
    ).

%task8
 generateColorsList([], 0) :- !.
 generateColorsList(Colors, ColorsCount) :-
    !, 
    NextCount is ColorsCount - 1,
    generateColorsList(NextColors, NextCount),
    Colors ~ [ColorsCount|NextColors].

 %тотально-переборный кринж
 getChromoNum(ColorsCount) :-
    readGraph((V,E), 'lab15_graph_input2.txt'),
    lenght(V,MaxColorsCount),
    between(0,(MaxColorsCount+1), ColorsCount),
    generateColorsList(Colors,ColorsCount),
    length(ColoredV,MaxColorsCount),
    ColoredV @: (
        [Vertex]>>(
            Colors <- Vertex
        )
    ),
    %лямбды наше все
    not(
        V ?: (
            [Vertex]>>(
                V ?: (
                    [Vertex2]>>(
                        not(Vertex is Vertex2),
                        (V,E): Vertex--Vertex2,
                        findIndex(V,Vertex,Ind1),
                        findIndex(V,Vertex2,Ind2),
                        getValue(ColoredV, Ind1, Clr1),
                        getValue(ColoredV, Ind2, Clr2),
                        Clr1 is Clr2
                    )
                )
            )
        )
    ),!. 

%task9
%красивое (не длинновое)
findMinEdgeCovering(SubEdges) :-
    readGraph((V,E),'lab15_graph_input2.txt'),
    E <= SubEdges,
    V @: (
        [Vertex]>>(
            (V,SubEdges): Vertex--_,!
        )
    ).