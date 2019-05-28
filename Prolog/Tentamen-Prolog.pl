factorial(0,1).
factorial(N,FN):-
 N1 is N - 1,
 factorial(N1,FN1),
 FN is FN1 * N,!.

factorials(0,[1]).
factorials(N,L):-
 N2 is N - 1,
 factorial(N,Res),
 factorials(N2,Res2),
 L2 = [Res|Res2], 
 insertionSort(L2, L), !.

%Plugga in!!
insert(X, [], [X]):- !.
insert(X, [X1|L1], [X, X1|L1]):- 
 X =< X1, !.
insert(X, [X1|L1], [X1|L]):- insert(X, L1, L).

insertionSort([], []):- !.
insertionSort([Head|Tail], Sorted):- insertionSort(Tail, Result), insert(Head, Result, Sorted).



f(_,[],0).

f(X,[X|T],N):-
 f(X,T,N2),
 N is N2 + 1,!.

f(X,[_|T],N):-
 f(X,T,N),!.

fL([],[]).
fL([H|T],FT):-
 f(H,[H|T],N),
 F1 = [H,N],
 delete(H,T),
 fL(T,F2),
 FT = [F1|F2],!. 


freqList(L,FL) :- setof([S,D],(member(S,L),freq(S,L,D)),FL).


freq(_,[],0) :- !. 
freq(X,[X|T],Num) :- freq(X,T,N), Num is N+1,!. % ! prevents backtracking into next case 
freq(X,[_|T],Num) :- freq(X,T,Num),!.



parent(z,a).
parent(t,b).
parent(m,z).
parent(m,t).

siblings(X,Y):-
 parent(Z,X),
 parent(Z,Y).

cousins(X,Y):-
 parent(Z,X),
 parent(Z1,Y),
 siblings(Z1,Z).


p(X,Y) :- !,a(X), b(Y).

p1(X,Y) :- a(X), !, b(Y).

p2(X,Y) :- a(X), !, b(Y).

p3(X,Y) :- a(X), b(Y),!.


t(X,Y,Z) :- !,a(X), b(Y),c(Z).

t1(X,Y,Z) :- a(X), !, b(Y),c(Z).

t2(X,Y,Z) :- a(X), b(Y),!,c(Z).

t3(X,Y,Z) :- a(X), b(Y),c(Z),!.

a(0).
a(1).

b(1).
b(0).

c(0).
c(1).



z:- !,true,true,true.
z1:- true,!,true,true.

fib(0,0).
fib(1,1).
fib(N,FN):-
 N1 is N - 1,
 N2 is N - 2,
 fib(N1,F1),
 fib(N2,F2),
 FN is F1 + F2,!.



fibs(0,[]).
fibs(N,L):-
 fib(N,Res),
 N1 is N - 1, 
 fibs(N1,L2),
 L3 = [Res|L2],
 sort(0, @=<, L3, L),!. 

:- dynamic arc/2.

arc(n1,n2).
arc(n2,n5).
arc(n2,n3).
arc(n3,n7).
arc(n3,n4).
arc(n6,n2).


path2(A,A,[]).
path2(A,B,[arc(A,Z)|P1]):-
 arc(A,Z),
 path2(Z,B,P1).

path(A,B,[arc(A,B)]) :- arc(A,B).%,!.
path(A,B,[arc(A,X) | R]) :-
arc(A,X),
path(X,B,R).%,!.

add(arc(X,X)):-!,fail.
add(arc(X,Y)):- path(Y,X,_),!,fail.
add(arc(X,Y)):- assert(arc(X,Y)).

allPaths(N,L) :-
 N2 is N-1,
 setof(X, A^B^(path(A,B,X),length(X,N2)), L).
