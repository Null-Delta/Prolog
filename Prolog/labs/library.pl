
:- op(2,xfy,@:).
:- op(2,xfy,#:).
:- op(2,xfy,?:).
:- op(2,xfy, <-).
:- op(2,xfy, </-).
:- op(6,xfy, ~).

L ~ L.
    
%проверка выполнения условия для всех элементов списка
%<список элементов> @: <предикат с условием>
[] @: _.
[H|T] @: F :-
    call(F, H),
    T @: F.

[] #: _.
[H|T] #: F :- 
    not(F ~ (A,B)),
    [H|T] #: (F, 0).

[H|T] #: (F, Index) :-
    call(F, H, Index),
    NewIndex is Index + 1,
    T #: (F, NewIndex).

%проверка выполнения условия для хотя бы одного элемента списка
%<список элементов> @: <предикат с условием>
[H|T] ?: F :-
    call(F, H),!;
    T ?: F,!.

%помер
%[E|T] <- [H1|T1] :- [H1|T1] @: ( [V]>>( [E|T]<-V ) ).

%проверяет содержится ли элемент в списке
%<список элементов> <- <элемент>
[E|_] <- E.
[_|T] <- E :- T <- E.

%проверяет не содержится ли элемент в списке
%<список элементов> </- <элемент>
List </- E :- not(List <- E).

writeResultInFile(Generator, FileName) :-
    (exists_file(FileName),delete_file(FileName); told ),
    tell(FileName),
    findall(_, Generator, _),
    told,!.

readLines(Lines, LocalLines) :-
    readString(X),
    (
        equalLists([],X),equalLists(Lines, LocalLines);
        pushBack(LocalLines, X, NewLocalLines),
        readLines(Lines,NewLocalLines)
    ).

readFile(File, Lines) :-
    see(File),
    readLines(Lines,[]).

%получить первое слово в строке и вернуть остаток строки
getFirstWord([], Word, _, Word).
getFirstWord([H|T], Word, Tail, LWord) :-
    (
        32 is H, Word = LWord, Tail = T;

        pushBack(LWord,H,NewLWord),
        getFirstWord(T,Word,Tail, NewLWord)
    ).
getFirstWord(List, Word, Tail) :- getFirstWord(List, Word, Tail, []).

%получить все слова в строке
getAllWords([], List, List).
getAllWords(String, List, LocalList) :-
    getFirstWord(String, Word, Tail),
    pushBack(LocalList, Word, NewLocalList),
    getAllWords(Tail, List, NewLocalList).
getAllWords(String, List) :- getAllWords(String, List, []).

writeAllWords([]).
writeAllWords([H|T]) :- writeString(H),nl, writeAllWords(T),!.

%обработка ввода
inputString(B,B, 10) :- !.
inputString(B,B, -1) :- !.
inputString(Str,LocalStr, Char) :-
    pushBack(LocalStr, Char, NewLocalStr),
    get0(X),
    inputString(Str, NewLocalStr, X),!.

%вывод строки
writeString([]) :- !.
writeString([H|T]) :- put(H), writeString(T).

%чтение строки
readString(String) :- get0(X),inputString(String,[], X).

equalLists(L,L).

contains([H|_],H).
contains([_|T], V) :- contains(T,V).

%получить значение по индексу
getValue([H|T],Index,X) :- 
    not(is_list(H)),
    (0 is Index, X ~ H; NewIndex is Index - 1,getValue(T,NewIndex,X)),!.
getValue([H|T],Index,X) :- 
    (0 is Index, equalLists(X, H); NewIndex is Index - 1,getValue(T,NewIndex,X)),!.
%разворот списка
reverseList([],Result, Local) :- Result = Local.
reverseList([H|T],Result, Local) :-
    reverseList(T, Result, [H|Local]).
reverseList(List,Result) :- reverseList(List,Result, []).

%добавить элемент в конец списка
pushBack(List,V,NewList) :-
    reverseList(List, ReversedList),
    reverseList([V|ReversedList], NewList).

%удалить элемент по индексу
removeByIndex([_|T], 0, NewList, LocalList) :-
    concatenate(LocalList, T, ConcatenatedList),
    equalLists(NewList, ConcatenatedList).

removeByIndex([H|T], Index, NewList, LocalList) :-
    NewIndex is Index - 1,
    pushBack(LocalList, H, NewLocalList),
    removeByIndex(T,NewIndex, NewList, NewLocalList).

removeByIndex(List, Index, NewList) :- removeByIndex(List, Index, NewList, []),!.

%удалить элемент
removeElement([H|T], H, NewList, LocalList) :-
    concatenate(LocalList, T, ConcatenatedList),
    equalLists(NewList, ConcatenatedList).

removeElement([H|T], Value, NewList, LocalList) :-
    pushBack(LocalList, H, NewLocalList),
    removeElement(T,Value, NewList, NewLocalList).

removeElement(List, Value, NewList) :- removeElement(List, Value, NewList, []),!.

%кастомный мап
mapList(_,[], []).
mapList(Func,[IH|IT], [OH|OT]) :-
    mapList(Func, IT, OT),
    call(Func,IH,OH),!.

%кастомный мап
filterList(_, [], Result, Result).
filterList(Predicate, [H|T], FilteredList, LocalList) :-
    call(Predicate, H),
    pushBack(LocalList, H, NewLocalList),
    filterList(Predicate, T, FilteredList, NewLocalList);
    filterList(Predicate,T,FilteredList, LocalList).
filterList(Predicate, List, FilteredList) :- filterList(Predicate, List, FilteredList, []).

%кастомный фолд
foldList(_,Result,[], Result) :- !.
foldList(Folder,State,[H|T], Output) :- 
    call(Folder, State, H, NewState),
    foldList(Folder, NewState, T, Output),!.

%объединение списков
concatenate(List1, [], List1).
concatenate(List1, [H|T], ResultList) :-
    pushBack(List1, H, NewList1),
    concatenate(NewList1,T,ResultList),!.

allList(_, []).
allList(Predicate, [H|T]) :-
    call(Predicate,H),
    allList(Predicate,T).

char_count_in_list(List,Value,Count) :-
    foldList(
        [State, V, NewState]>>(
            V is Value,NewState is State + 1; NewState is State
        ),
        0,
        List,
        Count
    ).

generateUniqueList([],R,R).
generateUniqueList([H|T], Result, LocalResult) :-
    not(contains(LocalResult,H)),
    pushBack(LocalResult, H, NewLocal),
    generateUniqueList(T,Result,NewLocal),!.
generateUniqueList([_|T], Result, LocalResult) :-
    generateUniqueList(T,Result,LocalResult),!.
generateUniqueList(List,Result) :- generateUniqueList(List, Result, []).

equal_chars_count(List,Count) :-
    generateUniqueList(List,UniqueList),
    length(UniqueList,Count).

all_chars_contrains(_,[]).
all_chars_contrains(List, [H2|T2]) :-
    contains(List,H2),
    all_chars_contrains(List,T2).

findIndex([H|T],Value,Index, LocalIndex) :-
    (H ~ Value, Index is LocalIndex;NewLocalIndex is LocalIndex + 1,findIndex(T,Value,Index, NewLocalIndex)).
findIndex(List,Value, Index) :- findIndex(List,Value,Index,0).

between(X,X,Z) :- !, fail.
between(X,Y,Z) :-
    Z is X + 1, Z < Y;
    NewX is X + 1,
    between(NewX, Y, Z).
