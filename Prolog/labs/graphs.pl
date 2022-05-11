:- ['library', 'combinatorics'].

%-----чтение------

readVertexes(V, Vs) :-
    getAllWords(Vs, Words),
    mapList(
        [Inp, Out]>>(
            string_to_atom(Inp, InpAtom),
            atom_number(InpAtom, Out)
        ),
        Words,
        V
    ),!.

readEdges([],[]).

readEdges(Edges,[H|T]) :-
    getAllWords(H,Words),
    mapList(
        [Inp, Out]>>(
            string_to_atom(Inp, InpAtom),
            atom_number(InpAtom, Out)
        ),
        Words,
        [V1,V2]
    ),
    readEdges(NextEdges,T),
    Edges ~ [(V1,V2) | NextEdges].
    

readGraph((V,E), File) :-
    readFile(File, [Vertexes|Edges]),
    readVertexes(V,Vertexes),
    readEdges(E,Edges),!.

%-----чтение------

:- op(1,xfy, :).
:- op(2,xfy, ~~).
:- op(6,xfy, </~).
:- op(4,xfy, --).
:- op(3,xfy, ~>).

% проверяет существует ли ребро V1-V2 в графе (V,E)
% <Граф с вершинами V и ребрами E> : <Первая вершина> -- <вторая вершина>
(_,E) : V1 -- V2 :- (E <- (V1,V2);E <- (V2,V1)).

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

% явзяется ли граф связанным
is_constrain((V,E)) :-
    V ~ [H|T],
    T @: ([Vertex]>>((V,E): H ~~ Vertex)),!.

% явзяется ли граф деревом
is_tree((V,E)) :-
    not(
        V ?: (
            [Vertex]>>(
                (V,E): Vertex--X,
                (V,E): Vertex--Y,
                X < Y,
                (V,E): X ~~ Y ~> Way,
                Way </- Vertex
            )
        )    
    ).