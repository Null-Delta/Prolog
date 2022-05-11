:- ['library', 'combinatorics'].

%-----чтение------

readWayVertexes(V, Vs) :-
    getAllWords(Vs, Words),
    mapList(
        [Inp, Out]>>(
            string_to_atom(Inp, InpAtom),
            atom_number(InpAtom, Out)
        ),
        Words,
        V
    ),!.

readWayEdges([],[]).

readWayEdges(Edges,[H|T]) :-
    getAllWords(H,Words),
    mapList(
        [Inp, Out]>>(
            string_to_atom(Inp, InpAtom),
            atom_number(InpAtom, Out)
        ),
        Words,
        [V1,V2,V3]
    ),
    readWayEdges(NextEdges,T),
    Edges ~ [(V1,V2,V3) | NextEdges].

readWay((V,E), File) :-
    readFile(File, [Vertexes|Edges]),
    readWayVertexes(V,Vertexes),
    readWayEdges(E,Edges),!.

%-----чтение------

:- op(1,xfy, :).
:- op(2,xfy, ~~).
:- op(6,xfy, </~).
:- op(4,xfy, --).
:- op(3,xfy, ~>).

% проверяет существует ли ребро V1-V2 в графе (V,E)
% <Граф с вершинами V и ребрами E> : <Первая вершина> -- <вторая вершина>
(_,E) : V1 -- V2 :- E <- (V1,V2,_).

% Находит все возможные простые пути из вершины V1 в вершину V2
% <Граф с вершинами V и ребрами E> : <Первая вершина> ~~ <вторая вершина> ~> <[путь, состоящий из номеров вершин]>
(_,_) : V1 ~~ V1 ~> [V1] </~ _.

(V,E) : V1 ~~ V2 ~> Way </~ UsedVertexes :-
    (V,E) : V1 -- X,
    UsedVertexes </- X,
    (V,E): X ~~ V2 ~> NextWay </~ [X|UsedVertexes],
    Way ~ [V1|NextWay].

(V,E) : V1 ~~ V2 ~> Way :- (V,E) : V1 ~~ V2 ~> Way </~ [V1].

% проверяет существует ли путь от вершины V1 в вершину V2
% <Граф с вершинами V и ребрами E> : <Первая вершина> ~~ <вторая вершина>
(V,E) : V1 ~~ V2 :- (V,E) : V1 ~~ V2 ~> _ </~ [V1].

lab15task10 :-
    readWay((V,E),'way.txt'),
    V2 ~ V,
    write(E),nl,
    length(E, N),
    length(E2, N),!,
    write(E2). 

fillWays(V,E,E2,Istok,Stok) :-
    Istok <- I,
    Stok <- S,

isPotok([],_,_,_,_).

% isPotok(V,E,W,UsedV,Cut, PotokSize) 

findIstok(V,E, Istok) :-
    filterList(
        [Val]>>(
            not((V,E): _ -- Val)
        ),
        V,
        Istok
    ),!.


findStok(V,E, Stok) :-
    filterList(
        [Val]>>(
            not((V,E): Val -- _)
        ),
        V,
        Stok
    ),!.

