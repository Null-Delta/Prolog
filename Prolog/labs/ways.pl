:- ['library', 'combinatorics'].

:- dynamic(vertex/1).
:- dynamic(edge/1).
:- dynamic(weights/2).
:- dynamic(wasFinded/0).

in_list([H|T],V) :-
    V is H;in_list(T,V).

%-----чтение------
readWayVertexes(Vs) :-
    getAllWords(Vs, Words),
    saveVertexes(Words).

saveVertexes([]).
saveVertexes([H|T]) :-
    name(Z,H),
    assertz(vertex(Z)),
    saveVertexes(T).

saveEdge([]).
saveEdge([H|T]) :-
    getAllWords(H,[V1,V2,W]),
    name(NV1,V1),
    name(NV2,V2),
    name(NW,W),
    assertz(edge([NV1,NV2,NW])),
    assertz(weights([NV1,NV2],0)),
    saveEdge(T).

readWay(File) :-
    readFile(File, [Vertexes|Edges]),
    readWayVertexes(Vertexes),
    saveEdge(Edges).

%-----чтение------

%найти истоки
get_inputs(Result, Local) :-
    vertex(X),
    not(contains(Local,X)),
    not(edge([_,X,_])),
    get_inputs(Result, [X|Local]).
get_inputs(X,X).
get_inputs(InputsList) :-
    get_inputs(InputsList,[]),!.

%найти стоки
get_outputs(Result, Local) :-
    vertex(X),
    not(contains(Local,X)),
    not(edge([X,_,_])),
    get_outputs(Result, [X|Local]).
get_outputs(X,X).
get_outputs(OutputsList) :-
    get_outputs(OutputsList,[]),!.

%найти путь
findWay(V1,V2,Way) :- findWay(V1,V2,Way,[V1]).

findWay(V1,V2,Way,VisitedVertexes) :-
    (edge([V1,V2,_]);edge([V2,V1,_])),
    Way ~ [V1,V2];
    (edge([V1,X,_]);edge([X,V1,_])),
    not(X ~ V2),
    not(contains(VisitedVertexes,X)),
    findWay(X,V2,SubWay,[X|VisitedVertexes]),
    Way ~ [V1|SubWay].

%можно ли пустить поток по пути
can_go([_]) :- !.
can_go([V1,V2|T]) :-
    (
        edge([V1,V2,W]),
        weights([V1,V2], FW),
        FW < W,
        can_go([V2|T]);

        edge([V2,V1,W]),
        weights([V2,V1], FW),
        FW < W,
        can_go([V2|T])
    ).

%поиск горла пути
findThroat([V1,V2],Min) :-
     (
        edge([V1,V2,W]), 
        weights([V1,V2],FW);
        edge([V2,V1,W]), 
        weights([V2,V1],FW)
     ), 
     Min is W - FW.

findThroat([V1,V2|T],Min) :-
    findThroat([V2|T],NextMin),
    (
        edge([V1,V2,W]),
        weights([V1,V2],FW);
        edge([V2,V1,W]),
        weights([V2,V1],FW)
    ),
    LW is W - FW,
    Min is min(LW,NextMin),!.

make_potok :-
    assertz(wasFinded),
    get_inputs(Inputs),
    get_outputs(Outputs),
    repeat,
    (
        not(wasFinded),!;
        retract(wasFinded),
        in_list(Inputs,I),
        in_list(Outputs,O),        
        findWay(I,O,Way),
        can_go(Way),
        findThroat(Way,Throat),
        push_potok(Way,Throat),
        assertz(wasFinded),
        fail
    ),
    print_potok. 

%пуск потока по пути
push_potok([_],Throat).
push_potok([V1,V2|T],Throat) :- 
    (
        weights([V1,V2],LW),
        retract(weights([V1,V2],LW)),
        NewW is LW + Throat,
        assertz(weights([V1,V2],NewW));

        weights([V2,V1],LW),
        retract(weights([V2,V1],LW)),
        NewW is LW - Throat,
        assertz(weights([V2,V1],NewW))
    ),
    push_potok([V2|T],Throat).

print_potok :-
    findall([Value,X],weights(Value,X),Weights),
    write(Weights),nl.

lab16_task10 :-
    readWay('way.txt'),
    make_potok,!.