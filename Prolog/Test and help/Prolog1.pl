



/**is_digesting(X,Y)  :-  just_ate(X,Y). 
is_digesting(X,Y)  :- 
    just_ate(X,Z), 
    is_digesting(Z,Y). 
    
just_ate(mosquito,blood(john)). 
just_ate(frog,mosquito). 
just_ate(stork,frog). */

member(X,[X|T]). 
   member(X,[H|T])  :-  member(X,T).


a2b([],[]).
a2b([a|Ta],[b|Tb])  :-  a2b(Ta,Tb).

add_3_and_double(X,Y)  :-  Y  is  (X+3)*2.

len([],0). 
   len([_|T],N)  :-  len(T,X),  N  is  X+1.

accLen([_|T],X,Y)  :-    Xnew  is  X+1,  accLen(T,Xnew,Y). 
   accLen([],X,X).

accMax([H|T],A,Max)  :- 
         H  >  A, 
         accMax(T,H,Max). 
    
accMax([H|T],A,Max)  :- 
    H  =<  A, 
    accMax(T,A,Max). 
    
accMax([],A,A).


max(List,Max)  :- 
             List  =  [H|_], 
             accMax(List,H,Max).
append2([],L,L). 
   append2([H|T],L2,[H|L3])  :-  append2(T,L2,L3).

%Sublists, prefix suffix , labP2.
prefix(P,L):-  append(P,_,L).

suffix(S,L):-  append(_,S,L).

sublist(SubL,L):-  suffix(S,L),  prefix(SubL,S).

%Reverse list.
reverselist([],[]).
reverselist([H|T],L) :- reverselist(T,NewL), append(NewL,[H],L).


accRev([H|T],Acc,List) :- accRev(T,[H|Acc],List).
accRev([],Acc,Acc).

%accRev([H|T],A,R):-  accRev(T,[H|A],R). 
%accRev([],A,A).

rev(L,R):-  accRev(L,[],R).