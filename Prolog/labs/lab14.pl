:- ['lab13','library','combinatorics'].

%task1l
task1_1 :- readString(X), writeString(X),write(','),writeString(X),write(','),writeString(X).
task1_2 :- readString(X), getAllWords(X,List),lenght(List,Y), write(Y),!.
task1_3 :- 
    readString(X),
    getAllWords(X,List),
    generateUniqueList(List, UniqueWords),
    generateList2(List, UniqueWords, CountOfWords),
    findMax(CountOfWords, Max),
    findIndex(CountOfWords, Max, Index),
    getValue(UniqueWords, Index, Value),
    writeString(Value),!.

writeChar(_,0).
writeChar(Char,Count) :- put(Char), NewCount is Count - 1, writeChar(Char, NewCount).

task1_4 :- 
    readString(X),
    lenght(X,L),
    (
        L =< 5,
        equalLists(X,[H1|_]),
        writeChar(H1,L);


        equalLists(X, [F1,F2,F3|_]),
        put(F1),put(F2),put(F3),

        reverseList(X, RevX),

        equalLists(RevX, [L1,L2,L3|_]),
        put(L3),put(L2),put(L1)
    ),!.

printIndex([],_,_).
printIndex([H|T], Value, Index) :-
    NewIndex is Index + 1,
    (
        H is Value,
        write(Index),write(' '),
        printIndex(T,Value,NewIndex);
        printIndex(T,Value,NewIndex)
    ).

task1_5 :- 
    readString(X),
    reverseList(X, RevX),
    equalLists(RevX, [LastChar|_]),
    printIndex(X,LastChar,0),!.

%task2
task2_1 :- 
    readFile('lab14_task2_1input.txt', Lines), 
    mapList(lenght, Lines, Lenghts),
    findMax(Lenghts, Max),
    write(Max),!.

haventSpace([]) :- !.
haventSpace([H|T]) :- haventSpace(T),not(32 is H),!.

task2_2 :-
    readFile('lab14_task2_2input.txt',Lines),
    filterList(
        haventSpace,
        Lines,
        WithoutSpaceLines),
    lenght(WithoutSpaceLines, Count),
    write(Count),!.

%oh shit, here we go again...lambda...
task2_3 :-
    readFile('lab14_task2_3input.txt',Lines),
    foldList(
        [State, Line, NewState]>>(
            foldList(
                [State2, Char, NewState2]>>(
                    (65 is Char;97 is Char),NewState2 is State2 + 1; NewState2 is State2
                ),
                0,
                Line,
                AInLine
            ),
            NewState is AInLine + State
        ),
        0,
        Lines,
        AInFile
    ),
    foldList(
        [State, Line, NewState]>>(
            lenght(Line,L),
            NewState is L + State
        ),
        0,
        Lines,
        CountOfChars
    ),
    AMid is AInFile / CountOfChars,
    filterList(
        [Line]>>(
            foldList(
                [State3, Char, NewState3]>>(
                    (65 is Char;97 is Char),NewState3 is State3 + 1; NewState3 is State3
                ),
                0,
                Line,
                AInLine
            ),
            AInLine > AMid
        ),
        Lines,
        Result
    ),
    writeAllWords(Result),!.

task2_4 :- 
    readFile('lab14_task2_4input.txt',Lines),
    foldList(
        [State, Line, NewState]>>(
            pushBack(Line,32,LineWithSpace),
            concatenate(State, LineWithSpace, NewState)
        ),
        [],
        Lines,
        ConcatenatedLines
    ),
    getAllWords(ConcatenatedLines, Words),
    calculateLists(Words, UniqueWords, Counts),
    findMax(Counts, Max),
    findIndex(Counts, Max, Index),
    getValue(UniqueWords,Index,Value),
    writeString(Value),!.

task2_5 :- 
    readFile('lab14_task2_5input.txt',Lines),
    foldList(
        [State, Line, NewState]>>(
            pushBack(Line,32,LineWithSpace),
            concatenate(State, LineWithSpace, NewState)
        ),
        [],
        Lines,
        ConcatenatedLines
    ),
    getAllWords(ConcatenatedLines, Words),
    calculateLists(Words, UniqueWords, _),
    (exists_file('lab14_task2_5output.txt'),delete_file('lab14_task2_5output.txt'); told ),
    tell('lab14_task2_5output.txt'),
    writeAllWords(UniqueWords),
    told,!.

%3
unsortList([], List, List).
unsortList(List, NewList, LocalList) :-
    lenght(List, L),
    random(0, L, RandIndex),
    getValue(List, RandIndex, Value),
    pushBack(LocalList, Value, NewLocalList),
    removeByIndex(List, RandIndex, NewSourceList),
    unsortList(NewSourceList, NewList, NewLocalList),!.
    
    
unsortList(List, NewList) :- unsortList(List, NewList, []).

task3_3 :-
    readFile('lab14_task3_3input.txt', [Line]),
    getAllWords(Line,Words),
    unsortList(Words, UnsortedWords),
    foldList(
        [State,Word,NewState]>>(
            concatenate(State,[32|Word],NewState)
        ),
        [],
        UnsortedWords,
        [_|ResultString]
    ),
    writeString(ResultString),!. 

task3_8 :-
    readFile('lab14_task3_3input.txt', [Line]),
    getAllWords(Line,Words),
    filterList(
        [Word]>>(
            lenght(Word,L),
            0 is L mod 2
        ),
        Words,
        FilteredWords
    ),
    lenght(FilteredWords,Result),
    write(Result),!.

task3_16 :-
    readFile('lab14_task3_16input.txt', Colors),
    filterList(
        [Word]>>(
            string_to_list('белый', Codes),
            equalLists(Word,Codes)
        ),
        Colors,
        Whites
    ),
    filterList(
        [Word]>>(
            string_to_list('синий', Codes),
            equalLists(Word,Codes)
        ),
        Colors,
        Blues
    ),
    filterList(
        [Word]>>(
            string_to_list('красный', Codes),
            equalLists(Word,Codes)
        ),
        Colors,
        Reds
    ),
    concatenate(Whites, Blues, PreResult),
    concatenate(PreResult, Reds, Result),
    writeAllWords(Result),!.

%task6
lab14task6_1(Chars,K) :-
    writeResultInFile(generateTask6_1List(Chars,K), 'lab14_task6_1output.txt').
    
generateTask6_1List(Chars,K) :-
    Count <? K,
    Chars /?~ Count ~~/ SelectedChars /~~ _,
    length(CharsCount,Count),
    CharsCount <<- K,
    length(List,K),
    List <~~ SelectedChars ~? CharsCount,
    write(List),nl.

lab14task6_2(Chars) :-
    writeResultInFile(generateTask6_2List(Chars), 'lab14_task6_2output.txt').
    
generateTask6_2List(Chars) :-
    length(Chars,N),
    Chars /?~ N ~~/ SelectedChars /~~ _,
    write(SelectedChars),nl.

lab14task6_3(Chars,K) :-
    writeResultInFile(generateTask6_3List(Chars,K), 'lab14_task6_3output.txt').
    
generateTask6_3List(Chars,K) :-
    Chars /?~ K ~~/ SelectedChars /~~ _,
    write(SelectedChars),nl.

lab14task6_4(Chars) :-
    writeResultInFile(generateTask6_4List(Chars), 'lab14_task6_4output.txt').
    
generateTask6_4List(Chars) :-
    Chars <= SubChars,
    write(SubChars),nl.

lab14task6_5(Chars,K) :-
    writeResultInFile(generateTask6_5List(Chars,K), 'lab14_task6_5output.txt').
    
generateTask6_5List(Chars,K) :-
    Chars /?- K --/ SelectedChars,
    write(SelectedChars),nl.

lab14task6_6(Chars,K) :-
    writeResultInFile(generateTask6_6List(Chars,K), 'lab14_task6_6output.txt').
    
generateTask6_6List(Chars,K) :-
    length(Chars,N),
    Cnt is min(K,N),
    CharsCount <? (Cnt + 1),
    length(SelectedChars, CharsCount),
    length(PlaceCounts,CharsCount),
    length(Result, K),
    Chars /?= CharsCount ==/ SelectedChars /== _,
    PlaceCounts <<- K,
    Result <== SelectedChars =? PlaceCounts,
    write(Result),nl.

%task7
lab14task7 :-
    writeResultInFile(generateTask7List(_), 'lab14_task7output.txt').
    
generateTask7List(List) :-
    Chars = [97,98,99,100,101,102],
    List ~ [_,_,_,_,_],
    Chars --/ 97 /-- CWA,
    CWA /?- 3 --/ [B,C,D],
    List <-- 97 -? 2,
    List <-- [B,C,D] -? 1,
    writeString(List),nl.

generateTask7List(List) :-
    Chars = [97,98,99,100,101,102],
    List ~ [_,_,_,_,_],
    Chars --/ 97 /-- CWA,
    CWA --/ B,
    List <-- 97 -? 2,
    List <-- B -? 3,
    writeString(List),nl.

generateTask7List(List) :-
    Chars = [97,98,99,100,101,102],
    List ~ [_,_,_,_,_],
    Chars /?- 2 --/ [97,B] /-- CWAB,
    CWAB --/ C,
    List <-- [97,B] -? 2,
    List <-- C,
    writeString(List),nl.

%task8
lab14task8 :-
    writeResultInFile(generateTask8List(_), 'lab14_task8output.txt').
    
generateTask8List(List) :-
    Chars = [97,98,99,100,101,102],
    List ~ [_,_,_,_,_],
    Chars --/ 97 /-- CWA,
    CWA /?- 3 --/ [B,C,D],
    List <-- 97 -? 2,
    List <-- [B,C,D] -? 1,
    writeString(List),nl.

%task9
lab14task9 :-
    writeResultInFile(generateTask9List(_), 'lab14_task9output.txt').
    
generateTask9List(List) :-
    Chars = [97,98,99,100,101,102],
    List ~ [_,_,_,_,_],
    Chars --/ A /-- CWA,
    CWA /?- 3 --/ [B,C,D],
    List <-- A -? 2,
    List <-- [B,C,D] -? 1,
    writeString(List),nl.

%task10
lab14task10 :-
    writeResultInFile(generateTask10List(_), 'lab14_task10output.txt').
    
generateTask10List(List) :-
    Chars = [97,98,99,100,101,102],
    List ~ [_,_,_,_,_,_],
    Chars /?- 2 --/ [A,B] /-- CWAB,
    CWAB /?- 2 --/ [C,D],
    List <-- [A,B] -? 2,
    List <-- [C,D] -? 1,
    writeString(List),nl.