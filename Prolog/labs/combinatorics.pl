:- ['library'].

% ———————No normal predicats?———————
% ⠀⣞⢽⢪⢣⢣⢣⢫⡺⡵⣝⡮⣗⢷⢽⢽⢽⣮⡷⡽⣜⣜⢮⢺⣜⢷⢽⢝⡽⣝
% ⠸⡸⠜⠕⠕⠁⢁⢇⢏⢽⢺⣪⡳⡝⣎⣏⢯⢞⡿⣟⣷⣳⢯⡷⣽⢽⢯⣳⣫⠇
% ⠀⠀⢀⢀⢄⢬⢪⡪⡎⣆⡈⠚⠜⠕⠇⠗⠝⢕⢯⢫⣞⣯⣿⣻⡽⣏⢗⣗⠏⠀
% ⠀⠀⠪⡪⡪⣪⢪⢺⢸⢢⢓⢆⢤⢀⠀⠀⠀⠀⠈⢊⢞⡾⣿⡯⣏⢮⠷⠁⠀⠀
% ⠀⠀⠀⠈⠊⠆⡃⠕⢕⢇⢇⢇⢇⢇⢏⢎⢎⢆⢄⠀⢑⣽⣿⢝⠲⠉⠀⠀⠀⠀
% ⠀⠀⠀⠀⠀⡿⠂⠠⠀⡇⢇⠕⢈⣀⠀⠁⠡⠣⡣⡫⣂⣿⠯⢪⠰⠂⠀⠀⠀⠀
% ⠀⠀⠀⠀⡦⡙⡂⢀⢤⢣⠣⡈⣾⡃⠠⠄⠀⡄⢱⣌⣶⢏⢊⠂⠀⠀⠀⠀⠀⠀
% ⠀⠀⠀⠀⢝⡲⣜⡮⡏⢎⢌⢂⠙⠢⠐⢀⢘⢵⣽⣿⡿⠁⠁⠀⠀⠀⠀⠀⠀⠀
% ⠀⠀⠀⠀⠨⣺⡺⡕⡕⡱⡑⡆⡕⡅⡕⡜⡼⢽⡻⠏⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
% ⠀⠀⠀⠀⣼⣳⣫⣾⣵⣗⡵⡱⡡⢣⢑⢕⢜⢕⡝⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
% ⠀⠀⠀⣴⣿⣾⣿⣿⣿⡿⡽⡑⢌⠪⡢⡣⣣⡟⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
% ⠀⠀⠀⡟⡾⣿⢿⢿⢵⣽⣾⣼⣘⢸⢸⣞⡟⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
% ⠀⠀⠀⠀⠁⠇⠡⠩⡫⢿⣝⡻⡮⣒⢽⠋⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
% ——————————————————————————————————

:- op(1,xfy, --/).
:- op(1,xfy, ~~/).
:- op(2,xfy, /--).
:- op(2,xfy, /~~).
:- op(3,xfy, /?-).
:- op(3,xfy, /?~).

:- op(4,xfy, <--).
:- op(5,xfy, -?).

:- op(4,xfy, <~~).
:- op(5,xfy, ~?).

:- op(7,xfy, ==/).
:- op(8,xfy, /==).
:- op(2,xfy, /=>).

:- op(9,xfy, /?=).
:- op(10,xfy, <==).
:- op(11,xfy, =?).

:- op(12,xfy, <<=).
:- op(13,xfy, <?).
:- op(14,xfy, <<-).
:- op(15,xfy, <=).
:- op(16,xfy, ?>).

_ <= [].
List <= X :-
    length(List,N),
    C <? N,
    List /?- C --/ _ /-- X.
List <= List.

%------операторы для работы с перестановками------

%выбрать один элемент из списка
% <cписок элементов> --/ <выбранный элемент>
[H|T] --/ Element :- Element ~ H; T --/ Element.

%выбрать один элемент из списка и вернуть список без данного элемента
% <cписок элементов> --/ <выбранный элемент> /-- <список без выбранного элемента>
List --/ Element /-- ListWithoutElement :- List --/ Element,removeElement(List,Element,ListWithoutElement).

%выбрать несколько элементов из списка
% <cписок элементов> /?- <кол-во элементов> --/ <выбранные элементы>
List /?- 1 --/ [Element] :- !,List --/ Element.
[H|T] /?- Count --/ Elements :- 
    NextCount is Count - 1,
    T /?- NextCount --/ NextList,
    Elements ~ [H|NextList];
    T /?- Count --/ Elements.

%выбрать несколько элементов из списка и вернуть список без данных элементов
% <cписок элементов> /?- <кол-во элементов> --/ <выбранные элементы> /-- <список без выбранных элементов>
List /?- 0 --/ [] /-- List :- !.
List /?- Count --/ Elements /-- ListWithoutElements :- 
    List /?- Count --/ Elements,
    foldList(
        [LastList, Element, NewList]>>(
            LastList --/ Element /--NewList
        ),
        List,
        Elements,
        ListWithoutElements
    ).

%размещяет элемент в списке
% <cписок элементов> <-- <размещяемый элемент> 
List <-- Element :-
    List ~ [H|_],
    var(H),
    H is Element;
    List ~ [_|T],
    T <-- Element.

%размещяет элементы в списке указаное количество раз
% <cписок элементов> <-- <размещяемые элементы>  -? <кол-во размещений>
_ <-- [] -? _ :- !.
List <-- [H|T] -? Count :-
    !,
    List ~ L,
    L <-- H -? Count,
    L <-- T -? Count.

%размещяет элемент в списке указаное количество раз
% <cписок элементов> <-- <размещяемый элемент>  -? <кол-во размещений>
List <-- Element -? 1 :- !,List <-- Element.
List <-- Element -? Count :-
    NextCount is Count - 1,
    List ~ [H|T],
    var(H),
    H is Element,
    T <-- Element -? NextCount;
    List ~ [_|T],
    T <-- Element -? Count.

%------операторы для работы с сочетаниями------

%выбрать один элемент из списка и вернуть список элементов, идущих после выбранного
% <cписок элементов> ==/ <выбранный элемент> /== <список оставшихся элементов>
[H|T] ==/ Element /=> LastElementsList :- 
    Element is H, LastElementsList ~ T;
    T ==/ Element /=> LastElementsList.


List ==/ Element /== ListWithoutElement :- List --/ Element,removeElement(List,Element,ListWithoutElement).

%выбрать несколько элементов из списка и вернуть список оставшихся элементов
% <cписок элементов> /?= <кол-во элементов> ==/ <выбранные элементы> /== <список оставшихся элементов>
List /?= 0 ==/ [] /== List :- !.
List /?= 1 ==/ [Element] /== ListWithoutElements :- !, List ==/ Element /== ListWithoutElements.
[H|T] /?= Count ==/ Elements /== ListWithoutElements :- 
    NextCount is Count - 1,
    T /?= NextCount ==/ NextList /== ListWithoutElements,
    Elements ~ [H|NextList];
    T /?= Count ==/ Elements /== ListWithoutElements.

%размещяет элемент в первую пустую ячейку
% <cписок элементов> <== <размещяемый элемент> 
List <== Element :-
    List ~ [H|_],
    var(H),
    H ~ Element,!;
    List ~ [_|T],
    T <== Element,!.

%размещяет элемент в первую пустую ячейку указанное количество ра-
% <cписок элементов> <== <размещяемый элемент> =? <количество размещений>
List <== Element =? 1 :- 
    !,
    not(is_list(Element)),
    List <== Element.
List <== Element =? Count :-
    not(is_list(Element)),
    NextCount is Count - 1,
    List ~ [H|T],
    var(H),
    H is Element,
    T <== Element =? NextCount,!;
    List ~ [_|T],
    T <== Element =? Count,!.

%размещяет элементы в первые пустые ячейки указанное количество раз
% <cписок элементов> <== <[размещяемые элементы]> =? <[количество размещений]>
_ <== [] =? [] :- !.
List <== [El] =? [Cnt] :- List <== El =? Cnt,!.
List <== [ElH|ElT] =? [CntH|CntT] :-
    List <== ElH =? CntH,
    List <== ElT =? CntT,!.

% получить натуральное число, меньшее указаного
0 <? _ :- fail.
X <? Y :-
    X is Y - 1;
    Y > 2,
    NewY is Y - 1,
    X <? NewY.

% получить натуральное число, большее указаного
X ?> Y :-
    X is Y + 1;
    NewY is Y + 1,
    X ?> NewY.

%Разместить Count единиц в список в порядке неубывания
[H|T] <<= Count :-
    atomic(Count),
    [H|T] <<= (Count,1).

[X] <<= (X,Y) :- Y =< X.
[H|T] <<= (Count,LastAdd) :-
    length([H|T], Length),
    MaxCount is Count - Length + 2,
    AddCount <? MaxCount,
    AddCount >= LastAdd,
    NewCount is Count - AddCount,
    H is AddCount,
    T <<= (NewCount, AddCount).

%Разместить Count единиц в список
[X] <<- X :- !.
[H|T] <<- Count :-
    length([H|T], Length),
    MaxCount is Count - Length + 2,
    AddCount <? MaxCount,
    NewCount is Count - AddCount,
    H is AddCount,
    T <<- NewCount.


%-----тест-----%
c(N,Chars) :-
    CharsCount <? N,
    length(SelectedChars, CharsCount),
    length(PlaceCounts,CharsCount),
    length(Result, N),
    Chars /?= CharsCount ==/ SelectedChars /== _,
    PlaceCounts <<- N,
    Result <== SelectedChars =? PlaceCounts,
    write(Result),nl.

% выбрать указанное кол-во элементов в случайном порядке
List /?~ 0 ~~/ [] /~~ List :- !.
List /?~ Count ~~/ Elements /~~ ListWithoutElements :- 
    List --/ Head /-- Last,
    NewCount is Count - 1,
    Last /?~ NewCount ~~/ Tail /~~ ListWithoutElements,
    Elements ~ [Head|Tail].

_ <~~ [] ~? [] :- !.
List <~~ [El] ~? [Cnt] :- !, List <-- El -? Cnt.
List <~~ [ElH|ElT] ~? [CntH|CntT] :-
    List <-- ElH -? CntH,
    List <~~ ElT ~? CntT.