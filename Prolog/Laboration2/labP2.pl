%working_directory(CWD, 'c:/users/marcus johansson/D7012E/Deklarativa-D7012E/Prolog/Laboration2' ).

take(N, _, Xs) :- N =< 0, !, N =:= 0, Xs = [].
take(_, [], []).
take(N, [X|Xs], [X|Ys]) :- M is N-1, take(M, Xs, Ys).

cutlast([H, H2 | T], [H |T2]) :- cutlast([H2 | T], T2).
cutlast([_], []).

sublist2([],Acc,Acc).
sublist2(L,Acc,SubL):-
 length(L,J),  
 makeList(L,[],X,1,J),
 cutlast(L,List),
 append(X,Acc,List2),
 sublist2(List,List2,SubL).

makeList([],Acc,Acc,_,_).
makeList(List,Acc,NewList,I,J):-
 List = [_|T],
 sumlist(List,Sum),
 Sub = sub(Sum,I,J,List),
 I2 is I + 1,
 makeList(T,[Sub|Acc],NewList,I2,J).

insert(X, [], [X]):- !.
insert(X, [X1|L1], [X, X1|L1]):- 
 X = sub(Sum,_,_,_),
 X1 = sub(Sum2,_,_,_),
 Sum =< Sum2, !.
insert(X, [X1|L1], [X1|L]):- insert(X, L1, L).


insertionSort([], []):- !.
insertionSort([X|L], S):- insertionSort(L, S1), insert(X, S1, S).

smallestKSets([],_):-
 write("Error, empty list").
smallestKSets(List,K):-
 sublist2(List,[],L),
 insertionSort(L,Sorted),
 take(K,Sorted,Y),
 print(List,Y).


printString([]). 
printString([H|T]):-
 H = sub(Sum,I,J,L),
 write(Sum),
 write('\t'),
 write(I),
 write('\t'),
 write(J),
 write('\t'),
 write(L),
 write('\n'),
 printString(T).


print(List,Y):-
 List2 = "Entered list: ",
 write(List2),
 write(List),
 write('\n \n Size \t i \t j \t sublist\n'),
 printString(Y).

gen(0,Acc,Acc).
gen(N,Acc,List):-
 T is N * ((-1)**N),
 N2 is N - 1,   
 gen(N2,[T|Acc],List). 
  

test1() :- smallestKSets([-1,2,-3,4,-5],3).
test2() :- gen(100,[],X), smallestKSets(X,15).
test3() :- smallestKSets([24,-11,-34,42,-24,7,-19,21],6).
test4() :- smallestKSets([3,2,-4,3,2,-5,-2,2,3,-3,2,-5,6,-2,2,3],8).
test5() :- smallestKSets([],3). 