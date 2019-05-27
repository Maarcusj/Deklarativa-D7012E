/* ----------------------------------------------------------
    CSE 3401 F12 Assignment 4 file

% Surname:
% First Name:
% Student Number:

  ------------------------------------------------------ */

%do not chagne the follwoing line!
:- ensure_loaded('play.pl').

%
% Tests
%

test1([[1,1,1,1,1,1],
       [1,1,1,1,1,1],
       [1,1,1,1,1,1],
       [.,.,.,.,.,.],
       [2,2,2,2,2,2],
       [2,2,2,2,2,2]]).
% test1(A), winner(A, B).
% test1(A), tie(A).
% test1(A), countStones(A, 0, 0, B, C).

test2([[.,.,.,.,.,.],
       [.,.,.,.,.,.],
       [.,.,1,2,.,.],
       [.,.,2,1,.,.],
       [.,.,2,.,.,.],
       [.,.,.,.,.,.]]).
% test2(A), moves(1, A, L).
% test2(A), validmove(1, A, [2,5]).

test3([[1,1,1,1,1,1],
       [1,2,2,2,2,1],
       [1,2,.,2,2,1],
       [1,2,2,2,2,1],
       [1,2,2,2,2,1],
       [1,1,1,1,1,1]]).
% test3(A), nextState(1,[2,2],A,B,C), showState(B).


% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set &get).
%       Your job is to implement several predicates using
%       these helpers (feel free to add your own helpers if needed,
%       MAKE SURE to write comments for all your helpers, marks will
%       be deducted for bad style!).
%
%       Implement the following predicates at their designated space
%       in this file (we suggest to have a look at file ttt.pl  to
%       see how the implementations is done for game tic-tac-toe.
%
%          * initialize(InitialState,InitialPlyr).
%          * winner(State,Plyr)
%          * tie(State)
%          * terminal(State)
%          * moves(Plyr,State,MvList)
%          * nextState(Plyr,Move,State,NewState,NextPlyr)
%          * validmove(Plyr,State,Proposed)
%          * h(State,Val)  (see question 2 in the handout)
%          * lowerBound(B)
%          * upperBound(B)
% /* ------------------------------------------------------ */







% /* ------------------------------------------------------ */

% We use the following State Representation:
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows:
%    . means the position is  empty
%    1 means player one has a stone in this position
%    2 means player two has a stone in this position.

human(1).
computer(2).
empty('.').
%empty(X) :- not(human(X)), not(computer(X)).

% given helper: Inital state of the board
initBoard([ [.,.,.,.,.,.], 
            [.,.,1,2,.,.],
          [.,.,1,2,.,.], 
          [.,.,2,1,.,.], 
            [.,.,.,.,.,.], 
          [.,.,.,.,.,.] ]).
/*            [.,.,1,1,.,.],
            [.,.,2,2,2,.], 
            [.,.,2,1,.,.], 
            [.,.,.,.,.,.], 
            [.,.,.,.,.,.] ]).*/

%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr).
%%%  holds iff InitialState is the initial state and
%%%  InitialPlyr is the player who moves first.

initialize(B, 1) :- initBoard(B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player

winner(State,Plyr):-
 checkMoves(State),
 countStones(State,0,0, P1 ,P2),
 (Plyr = 1, P1 < P2;
  Plyr = 2, P2 < P1).
  /*write('Human has '),
  write(P1O),
  writeln(' stones'),
  write('Computer has '),
  write(P2O),
  writeln(' stones').*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here.
%    - true if terminal State is a "tie" (no winner)

tie(State) :- checkTie(State).

checkTie(State) :-
  checkMoves(State),
  countStones(State, 0, 0, P1, P2),
  (P1 = P2 -> true ; fail).

checkMoves(State) :- 
moves(1, State, MvList1), !, 
MvList1 = [n], !,
moves(2, State, MvList2), !, 
MvList2 = [n], !.

countStones(State,Acc1,Acc2, P1 ,P2) :-
 getStones(State,0,0,[],[],S1,S2),
 length(S1,P1),
 length(S2,P2).

getStones(State,_,6,Acc1,Acc2,Acc1,Acc2).
getStones(State,X,Y,Acc1,Acc2,P1,P2):-
 getRow(State,X,Y,[],[],P11,P22),
 append(P11,Acc1,A1),
 append(P22,Acc2,A2),
 Y1 is Y + 1,
 getStones(State,X,Y1,A1,A2,P1,P2).

getRow(State,6,Y,Acc1,Acc2,Acc1,Acc2).
getRow(State,X,Y,Acc1,Acc2,P1,P2):-
 X1 is X + 1,
 get(State,[X,Y],R),
 (R == 1,
 A1 = [[X,Y]|Acc1],
 getRow(State,X1,Y,A1,Acc2,P1,P2);
 R == 2 ,
 A2 = [[X,Y]|Acc2],
 getRow(State,X1,Y,Acc1,A2,P1,P2);
 R == '.',
 getRow(State,X1,Y,Acc1,Acc2,P1,P2)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State).
%   - true if State is a terminal

terminal(State) :- checkMoves(State), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%showState(State)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% given helper. DO NOT  change this. It's used by play.pl
%%
showState( G ) :-
  printList(['  ',0,1,2,3,4,5]),
  nl,
  printList([' ','____________']),
  nl,
	printRows( G,0).

printRows( [],_ ).
printRows( [H|L] , N) :-
  N2 is N + 1 ,
  write(N),
  write('|'),
  write(' '),
	printList(H),
	nl,
	printRows(L,N2).

printList([]).
printList([H | L]) :-
	write(H),
	write(' '),
	printList(L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves(Plyr,State,MvList)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define moves(Plyr,State,MvList).
%   - returns list MvList of all legal moves Plyr can make in State
%

moves(Plyr,State,MvList) :-
 getStones(State,0,0,[],[],P1,P2),
 (Plyr = 1 -> help(State,Plyr,P1,[],List);
  Plyr = 2 -> help(State,Plyr,P2,[],List)),
 (List = [] -> MvList = [n];
 removeDouble(List,[],List2),
 insertionSort(List2,MvList)),!.


help(State,Plyr,[],Acc,Acc):- !.
help(State,Plyr,[P1|P2],Acc,MvList):-
 validDir(P1,List),
 validHelp(State,Plyr,List,P1,[],Mv),
 append(Mv,Acc,Acc2),
 help(State,Plyr,P2,Acc2,MvList).

validHelp(State,Plyr,[],P1,Acc,Acc):-!.
validHelp(State,Plyr,[H1|T1],P1,Acc,MvList):-
 checkValid(State,H1,P1,Plyr,[],List),
 (List = [] -> validHelp(State,Plyr,T1,P1,Acc,MvList); 
 validHelp(State,Plyr,T1,P1,[List|Acc],MvList)).


checkValid(State,Dr,[],Plyr,Acc,Acc):- !.
checkValid(State,Dr,[],Plyr,[],[]):- !.
checkValid(State,Dr,[X,Y],Plyr,Acc,Valid):-
 X1 is X - 1,X2 is X + 1,Y1 is Y - 1,Y2 is Y + 1,
 (Dr = n -> D = [X,Y1];
  Dr = ne -> D = [X2,Y1];
  Dr = e -> D = [X2,Y];
  Dr = se -> D = [X2,Y2];
  Dr = s -> D = [X,Y2];
  Dr = sw -> D = [X1,Y2];
  Dr = w -> D = [X1,Y];
  Dr = nw-> D = [X1,Y1]),
 
 (checkBoundary(D) -> 
          get(State, D, Value),
          (Value == '.' , Acc = [] -> checkValid(State,Dr,[],Plyr,Acc,Valid);
          Value == '.' , Acc \= [] -> checkValid(State,Dr,[],Plyr,D,Valid); 
          Value == Plyr -> checkValid(State,Dr,[],Plyr,[],Valid);
          checkValid(State,Dr,D,Plyr,D,Valid));
          checkValid(State,Dr,[],Plyr,[],[])).

validDir([X,Y],List):-
 (X = 0 , Y = 0 -> List = [e,se,s];
  X = 5 , Y = 0 -> List = [w,sw,s];
  X = 0 , Y = 5 -> List = [n,ne,e];
  X = 5 , Y = 5 -> List = [n,nw,w];
  X = 0 -> List = [n,ne,e,se,s];
  Y = 0 -> List = [w,e,sw,se,s];
  X = 5 -> List = [n,nw,w,sw,s];
  Y = 5 -> List = [w,nw,n,ne,e];
  List = [n,ne,e,se,s,sw,w,nw]).

insert(X, [], [X]):- !.
insert(X, [X1|L1], [X, X1|L1]):- 
 X = [X3,Y3],
 X1 = [X4,Y4],
 (Y3 < Y4; Y3 = Y4 , X3 < X4), !.

insert(X, [X1|L1], [X1|L]):- insert(X, L1, L).


insertionSort([], []):- !.
insertionSort([X|L], S):- insertionSort(L, S1), insert(X, S1, S).

removeDouble([],Acc,Acc).
removeDouble([H|T],Acc,NewList):-
 (member(H,T) -> removeDouble(T,Acc,NewList); 
 removeDouble(T,[H|Acc],NewList)).

%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%%
%% define nextState(Plyr,Move,State,NewState,NextPlyr).
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next
%     state) and NextPlayer (i.e. the next player who will move).
% 

nextState(2,n,State,State,1):- !.
nextState(1,n,State,State,2):- !.
nextState(2,Move,State,NewState,1):-
 set(State,NS,Move,2),
 setNextStone(NS,Move,2,NewState1,n),
 setNextStone(NewState1,Move,2,NewState2,nw),
 setNextStone(NewState2,Move,2,NewState3,ne),
 setNextStone(NewState3,Move,2,NewState4,e),
 setNextStone(NewState4,Move,2,NewState5,se),
 setNextStone(NewState5,Move,2,NewState6,s),
 setNextStone(NewState6,Move,2,NewState7,sw),
 setNextStone(NewState7,Move,2,NewState,w).

nextState(1,Move,State,NewState,2):-
 set(State,NS,Move,1),
 setNextStone(NS,Move,1,NewState1,n),
 setNextStone(NewState1,Move,1,NewState2,nw),
 setNextStone(NewState2,Move,1,NewState3,ne),
 setNextStone(NewState3,Move,1,NewState4,e),
 setNextStone(NewState4,Move,1,NewState5,se),
 setNextStone(NewState5,Move,1,NewState6,s),
 setNextStone(NewState6,Move,1,NewState7,sw),
 setNextStone(NewState7,Move,1,NewState,w).

getDir([X,Y],Dr,D1,D2,D3):-
  X1 is X - 1,X2 is X + 1,Y1 is Y - 1,Y2 is Y + 1,
  X3 is X - 2,X4 is X + 2,Y3 is Y - 2,Y4 is Y + 2,
 (Dr = n  -> D1 = [X,Y1] , D2 = [X,Y3]  ,D3 = [0,-1];
  Dr = nw -> D1 = [X1,Y1], D2 = [X3,Y3] ,D3 = [-1,-1];
  Dr = ne -> D1 = [X2,Y1], D2 = [X4,Y3] ,D3 = [1,-1];
  Dr = e  -> D1 = [X2,Y] , D2 = [X4,Y]  ,D3 = [1,0];
  Dr = se -> D1 = [X2,Y2], D2 = [X4,Y4] ,D3 = [1,1];
  Dr = s  -> D1 = [X,Y2] , D2 = [X,Y4]  ,D3 = [0,1];
  Dr = sw -> D1 = [X1,Y2], D2 = [X3,Y4] ,D3 = [-1,1];
  Dr = w  -> D1 = [X1,Y] , D2 = [X3,Y]  ,D3 = [-1,0]).

checkBoundary([X, Y]) :- X >= 0, X =< 5, Y >= 0, Y =< 5.

setNextStone(State,[X,Y],Plyr,NewState,Dir):-
 getDir([X,Y],Dir,D1,D2,D3),
 (\+checkBoundary(D2)),
 NewState = State.

setNextStone(State,[X,Y],Plyr,NewState,Dir):-
 getDir([X,Y],Dir,D1,D2,D3),
 checkBoundary(D1),
 checkBoundary(D2),
 get(State, D1, Value1),
 get(State, D2, Value2),
 Value1 \= Value2,
 Plyr == Value2,
 Value1 \= '.',
 set(State, NewState1, D1, Plyr),
 set(NewState1, NewState2, D1, Plyr),
 D3 = [X1,X2],
 Dir2 = [-X1,-X2],
 setStonesBack(NewState2,NewState,D1,Plyr,Dir2).

setNextStone(State,[X,Y],Plyr,NewState,Dir):-
 getDir([X,Y],Dir,D1,D2,D3),
 checkBoundary(D1),
 checkBoundary(D2),
 get(State, D1, Value1),
 get(State, D2, Value2),
 Value1 == '.',
 NewState = State.

setNextStone(State,[X,Y],Plyr,NewState,Dir):-
 getDir([X,Y],Dir,D1,D2,D3),
 checkBoundary(D1),
 checkBoundary(D2),
 get(State, D1, Value1),
 get(State, D2, Value2),
 Value1 == Plyr,
 NewState = State.

setNextStone(State,[X,Y],Plyr,NewState,Dir):-
 getDir([X,Y],Dir,D1,D2,D3),
 checkBoundary(D1),
 checkBoundary(D2),
 get(State, D1, Value1),
 get(State, D2, Value2),
 Value1 \= Plyr,
 Value2 = '.',
 NewState = State.

setNextStone(State,[X,Y],Plyr,NewState,Dir):-
 getDir([X,Y],Dir,D1,D2,D3),
 checkBoundary(D1),
 checkBoundary(D2),
 get(State, D1, Value1),
 get(State, D2, Value2),
 Value1 == Value2,
 Plyr \= Value2,
 Value1 \= '.',
 setNextStone(State,D1,Plyr,NewState,Dir).
setNextStone(State,_,_,State_):- !.


setStonesBack(State,State,[X1,Y1],Plyr,[X2,Y2]):-
 X is X1+X2, 
 Y is Y1+Y2,
 get(State,[X,Y], P1),
 P1 == Plyr.

setStonesBack(State,NewState,[X1,Y1],Plyr,[X2,Y2]):-
 X is (X1+X2), 
 Y is (Y1+Y2),
 set(State, NewState1 , [X, Y], Plyr),
 setStonesBack(NewState1,NewState,[X,Y],Plyr,[X2,Y2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%%
%% define validmove(Plyr,State,Proposed).
%   - true if Proposed move by Plyr is valid at State.

validmove(Plyr,State,n):- moves(Plyr, State, M), !, M = [n]. 
validmove(Plyr,State,Proposed):-
 moves(Plyr,State,MvList),
 member(Proposed,MvList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define h(State,Val).
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.

h(State,1000) :- winner(State,1), !.
h(State,-1000) :- winner(State,2), !.
h(State,0) :- tie(State), !.
h(State, Val) :- 
 countStones(State, 0, 0, P1, P2), Val is (P2 - P1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define lowerBound(B).
%   - returns a value B that is less than the actual or heuristic value
%     of all states.

lowerBound(-1001).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define upperBound(B).
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.

upperBound(1001).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
%                                                                       %
%                Given   UTILITIES                                      %
%                   do NOT change these!                                %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get(Board, Point, Element)
%    : get the contents of the board at position column X and row Y
% set(Board, NewBoard, [X, Y], Value):
%    : set Value at column X row Y in Board and bind resulting grid to NewBoard
%
% The origin of the board is in the upper left corner with an index of
% [0,0], the upper right hand corner has index [5,0], the lower left
% hand corner has index [0,5], the lower right hand corner has index
% [5,5] (on a 6x6 board).
%
% Example
% ?- initBoard(B), showState(B), get(B, [2,3], Value).
%. . . . . .
%. . . . . .
%. . 1 2 . .
%. . 2 1 . .
%. . . . . .
%. . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'],
%     ['.', '.', 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...],
%     ['.', '.', '.', '.'|...], ['.', '.', '.'|...]]
%Value = 2
%Yes
%?-
%
% Setting values on the board
% ?- initBoard(B),  showState(B),set(B, NB1, [2,4], 1), set(NB1, NB2, [2,3], 1),  showState(NB2).
%
% . . . . . .
% . . . . . .
% . . 1 2 . .
% . . 2 1 . .
% . . . . . .
% . . . . . .
%
% . . . . . .
% . . . . . .
% . . 1 2 . .
% . . 1 1 . .
% . . 1 . . .
% . . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.',
%1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', '.', '.'|...], ['.', '.',
% '.'|...]]
%NB1 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', '.
%', '.'|...]]
%NB2 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 1, 1, '.'|...], ['.', '.', 1, '.'|...], ['.',
%'.', '.'|...]]

% get(Board, Point, Element): get the value of the board at position
% column X and row Y (indexing starts at 0).
get( Board, [X, Y], Value) :-
	nth0( Y, Board, ListY),
	nth0( X, ListY, Value).

% set( Board, NewBoard, [X, Y], Value)

set( [Row|RestRows], [NewRow|RestRows], [X, 0], Value)
    :- setInList(Row, NewRow, X, Value).

set( [Row|RestRows], [Row|NewRestRows], [X, Y], Value) :-
	Y > 0,
	Y1 is Y-1,
	set( RestRows, NewRestRows, [X, Y1], Value).

% setInList( List, NewList, Index, Value)

setInList( [_|RestList], [Value|RestList], 0, Value).

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :-
	Index > 0,
	Index1 is Index-1,
	setInList( RestList, NewRestList, Index1, Value).
