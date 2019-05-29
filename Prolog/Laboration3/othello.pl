/* ------------------------------------------------------- */
%
%    D7012E Declarative languages
%    Luleå University of Technology
%
%    Student full name: Marcus Johansson
%    Student user id  : johmah-3
%
/* ------------------------------------------------------- */

%do not chagne the follwoing line!
:- ensure_loaded('play.pl').
:- ensure_loaded('testboards.pl').
:- ensure_loaded('stupid.pl').
:- ensure_loaded('rndBoard.pl').

%
% Tests
%
%working_directory(CWD, 'c:/users/johan/Desktop/D7012E/Deklarativa-D7012E/Prolog/Laboration3' ).
%consult('othello.pl').

%play.

%tieInTwoMovesFullBoard(InitState), playgame(1,InitState).
%winInTwoMovesFullBoard(InitState), playgame(1,InitState).
%forcing1toDoNullMoves(InitState), playgame(1,InitState).

%rndBoardXYZ(InitialState),playgame(1,InitState).
%rndBoardXYZ(InitialState),playgame(1,InitState).
%rndBoardXYZ(InitialState),playgame(1,InitState).

%Change: initialize(InitialState, 1) :- rndBoardXYZ(InitialState).
%consult('othello.pl').
%consult('stupid.pl').


%Change back: initialize(B, 2) :- initBoard(B).
%%consult('stupid.pl').
%play.



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set &get).
%       Your job is to implement several predicates using
%       these helpers. Feel free to add your own helpers if
%       needed, as long as you write comments (documentation)
%       for all of them. 
%
%       Implement the following predicates at their designated
%       space in this file. You might like to have a look at
%       the file  ttt.pl  to see how the implementations is
%       done for game tic-tac-toe.
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

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% We use the following State Representation: 
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows: 
%    . means the position is  empty
%    1 means player one has a stone in this position
%    2 means player two has a stone in this position. 





% DO NOT CHANGE THE COMMENT BELOW.
%
% given helper: Inital state of the board
initBoard([ [.,.,.,.,.,.], 
            [.,.,.,.,.,.],
          [.,.,1,2,.,.], 
          [.,.,2,1,.,.], 
            [.,.,.,.,.,.], 
          [.,.,.,.,.,.] ]).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr). 
%%%  holds iff InitialState is the initial state and 
%%%  InitialPlyr is the player who moves first. 

  
initialize(B, 1) :- initBoard(B).
initialize(B, 2) :- initBoard(B).
%initialize(InitialState, 1) :- rndBoardXYZ(InitialState).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.  
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player 

%Check if both players need to pass.
%Count stones and return a winner which have the least stones on board.
winner(State,Plyr):-
 checkMoves(State),
 countStones(State,0,0, P1 ,P2),
 (Plyr = 1, P1 < P2;
  Plyr = 2, P2 < P1).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 

%Check if # stones are equal.
tie(State) :- checkTie(State).

%Check if both players only can pass.
%Count # stones.
%If equal return true.
checkTie(State) :-
  checkMoves(State),
  countStones(State, 0, 0, P1, P2),
  (P1 = P2 -> true ; fail).

%Returns true if both have only pass as move left.
checkMoves(State) :- 
 moves(1, State, MvList1), 
 MvList1 = [n], !,
 moves(2, State, MvList2), 
 MvList2 = [n], !.

/**
countStones: 
- calls getStones wich returns two lists with coordinates of every stone for each player value.
- Check length of each list and return # stones in each list.  

getStones:
- Calls getRow hich returns two list with coordinates of each stones on each row.
- Appends two accumulators and calls next row recursive.
- Helper to moves.

getRow:
- Calls get(Board,cell,value), and retrive value of specific cell.
- Append coordinates of that cell to list.
- Recursive with accumelator and returns two lists with stones on each row. 
 
*/

countStones(State,_,_, P1 ,P2) :-
 getStones(State,0,0,[],[],S1,S2),
 length(S1,P1),
 length(S2,P2).

getStones(_,_,6,Acc1,Acc2,Acc1,Acc2).
getStones(State,X,Y,Acc1,Acc2,P1,P2):-
 getRow(State,X,Y,[],[],P11,P22),
 append(P11,Acc1,A1),
 append(P22,Acc2,A2),
 Y1 is Y + 1,
 getStones(State,X,Y1,A1,A2,P1,P2).

getRow(_,6,_,Acc1,Acc2,Acc1,Acc2).
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

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal  

%Check if terminal state is reached.
terminal(State) :- checkMoves(State), !.

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
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

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves(Plyr,State,MvList)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define moves(Plyr,State,MvList). 
%   - returns list MvList of all legal moves Plyr can make in State
%
/**
moves:
- Get stones for each player saved in two lists with coordinates with getStones.
- Check which player and list to pass forward to rule help.
- removes doubles of possible moves.
- sort them left corner to right, then by row.  

help:
- Calls validDir to get possible move directions for first stone in list.
- calls validHelp to with first of stones for that player.
- gets valid moves and append them to accumelator.
- Calls recurivly help for next stones in list of player stones.
validHelp:
- Calls check valid for each direction of stone.
- If empty list returned, dont append.
- Calles recurivly for every element in direction list. 
checkValid:
- Input coordinates and valid directions and calculate cells that should be checked for valid moves.
- Get value of specific cell. 
- Check if it's empty,oponent value or player value.
- if empty and next is oponent and next is player value, its valid.
- Calls checkvalid to see next stones after one. 
- If a corner or border is reached , checkboundery returns false.
validDir:
- returns valid directions for each stone.
- This is used to know which direction stones after should check. 
*/
moves(Plyr,State,MvList) :-
 getStones(State,0,0,[],[],P1,P2),
 (Plyr = 1 -> help(State,Plyr,P1,[],List);
  Plyr = 2 -> help(State,Plyr,P2,[],List)),
 (List = [] -> MvList = [n];
 removeDouble(List,[],List2),
 insertionSort(List2,MvList)),!.


help(_,_,[],Acc,Acc):- !.
help(State,Plyr,[P1|P2],Acc,MvList):-
 validDir(P1,List),
 validHelp(State,Plyr,List,P1,[],Mv),
 append(Mv,Acc,Acc2),
 help(State,Plyr,P2,Acc2,MvList).

validHelp(_,_,[],_,Acc,Acc):-!.
validHelp(State,Plyr,[H1|T1],P1,Acc,MvList):-
 checkValid(State,H1,P1,Plyr,[],List),
 (List = [] -> validHelp(State,Plyr,T1,P1,Acc,MvList); 
 validHelp(State,Plyr,T1,P1,[List|Acc],MvList)).


checkValid(_,_,[],_,Acc,Acc):- !.
checkValid(_,_,[],_,[],[]):- !.
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


/**
- Classic insertionsort.
*/
insert(X, [], [X]):- !.
insert(X, [X1|L1], [X, X1|L1]):- 
 X = [X3,Y3],
 X1 = [X4,Y4],
 (Y3 < Y4; Y3 = Y4 , X3 < X4), !.

insert(X, [X1|L1], [X1|L]):- insert(X, L1, L).


insertionSort([], []):- !.
insertionSort([X|L], S):- insertionSort(L, S1), insert(X, S1, S).


%Helper to remove doubles in MvList.
%Could modify moves to remove this.  
removeDouble([],Acc,Acc).
removeDouble([H|T],Acc,NewList):-
 (member(H,T) -> removeDouble(T,Acc,NewList); 
 removeDouble(T,[H|Acc],NewList)).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%% 
%% define nextState(Plyr,Move,State,NewState,NextPlyr). 
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).
%
/**
nextState:
- Return same state if move is a pass and change current player.
- Calls setNextStone for all directions around a cell.
- Sends nextState into next setNextStone. 
- Returns final state after every direction is checked.

setNextStone:
- Called for every direction of cell which is to be placed.
- Have checks for every condition a cell can have. 
- Checks two stones forward in evey direction.
- This makes it possible to see when a new player value is reached and all between should be flipped.
- Uses setStoneBack to flip previous visit stones when flip should be preformed. 

getDir:
- returns coordinates for two stones forward in the correct direction.

setStonesBack:
- when a state when current player have surround oponent stones a flip of every between is made.
- This rule is used to go back and flip them. 
- This was a major problem until this solution. 

*/
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

setNextStone(State,[X,Y],_,NewState,Dir):-
 getDir([X,Y],Dir,_,D2,_),
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

setNextStone(State,[X,Y],_,NewState,Dir):-
 getDir([X,Y],Dir,D1,D2,_),
 checkBoundary(D1),
 checkBoundary(D2),
 get(State, D1, Value1),
 get(State, D2, _),
 Value1 == '.',
 NewState = State.

setNextStone(State,[X,Y],Plyr,NewState,Dir):-
 getDir([X,Y],Dir,D1,D2,_),
 checkBoundary(D1),
 checkBoundary(D2),
 get(State, D1, Value1),
 get(State, D2, _),
 Value1 == Plyr,
 NewState = State.

setNextStone(State,[X,Y],Plyr,NewState,Dir):-
 getDir([X,Y],Dir,D1,D2,_),
 checkBoundary(D1),
 checkBoundary(D2),
 get(State, D1, Value1),
 get(State, D2, Value2),
 Value1 \= Plyr,
 Value2 = '.',
 NewState = State.

setNextStone(State,[X,Y],Plyr,NewState,Dir):-
 getDir([X,Y],Dir,D1,D2,_),
 checkBoundary(D1),
 checkBoundary(D2),
 get(State, D1, Value1),
 get(State, D2, Value2),
 Value1 == Value2,
 Plyr \= Value2,
 Value1 \= '.',
 setNextStone(State,D1,Plyr,NewState,Dir).
%setNextStone(State,_,_,State_):- !.


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


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%% 
%% define validmove(Plyr,State,Proposed). 
%   - true if Proposed move by Plyr is valid at State.

/**
validmove: 
- Check if the move to be preformed is a valid move.
- Uses moves to get moves that can be made.
- Check if proposed move is a member in MvList. 

*/
validmove(Plyr,State,n):- moves(Plyr, State, MvList),  MvList = [n], !. 
validmove(Plyr,State,Proposed):-
 moves(Plyr,State,MvList),
 member(Proposed,MvList),!.

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define h(State,Val). 
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.
/**
H: 
- winning state gives val 1000 for player 1.
- winning state gives val -1000 for player 2.
- tie state gives val 0.
- Place in Corners are given a value at 400 and two are given 500.
- Makes computer avoid corners. 
- Should add for player 1 also? If remove and only return actual value.  
*/
h(State,1000) :- winner(State,1), !.
h(State,-1000) :- winner(State,2), !.
h(State,0) :- tie(State), !.

h(State,Val):- get(State,[0,0],Elem1),Elem1 == 2 ,countStones(State, 0, 0, P1, P2),Val is 400 + (P2 - P1).

h(State,Val):- get(State,[0,0],Elem1),get(State,[0,5],Elem2),Elem2 == 2 ,Elem1 == 2 ,countStones(State, 0, 0, P1, P2),Val is 500 + (P2 - P1).
h(State,Val):- get(State,[0,0],Elem1),get(State,[5,0],Elem2),Elem2 == 2 ,Elem1 == 2 ,countStones(State, 0, 0, P1, P2),Val is 500 + (P2 - P1).
h(State,Val):- get(State,[5,5],Elem1),get(State,[0,5],Elem2),Elem2 == 2 ,Elem1 == 2 ,countStones(State, 0, 0, P1, P2),Val is 500 + (P2 - P1).
h(State,Val):- get(State,[5,5],Elem1),get(State,[5,0],Elem2),Elem2 == 2 ,Elem1 == 2 ,countStones(State, 0, 0, P1, P2),Val is 500 + (P2 - P1).
h(State,Val):- get(State,[5,5],Elem1),get(State,[0,0],Elem2),Elem2 == 2 ,Elem1 == 2 ,countStones(State, 0, 0, P1, P2),Val is 600 + (P2 - P1).

h(State,Val):-get(State,[0,5],Elem1),Elem1 == 2 ,countStones(State, 0, 0, P1, P2),Val is 400 + (P2 - P1).

h(State,Val):-get(State,[5,0],Elem1),Elem1 == 2 ,countStones(State, 0, 0, P1, P2),Val is 400 + (P2 - P1).

h(State,Val):-get(State,[5,5],Elem1),Elem1 == 2 ,countStones(State, 0, 0, P1, P2),Val is 400 + (P2 - P1).

%Tests
/*h(State,Val):- get(State,[0,0],Elem1),Elem1 == 1 ,countStones(State, 0, 0, P1, P2),Val is 400 + (P2 - P1).

h(State,Val):- get(State,[0,0],Elem1),get(State,[0,5],Elem2),Elem2 == 1 ,Elem1 == 1 ,countStones(State, 0, 0, P1, P2),Val is -500 + (P2 - P1).
h(State,Val):- get(State,[0,0],Elem1),get(State,[5,0],Elem2),Elem2 == 1 ,Elem1 == 1 ,countStones(State, 0, 0, P1, P2),Val is -500 + (P2 - P1).
h(State,Val):- get(State,[5,5],Elem1),get(State,[0,5],Elem2),Elem2 == 1 ,Elem1 == 1 ,countStones(State, 0, 0, P1, P2),Val is -500 + (P2 - P1).
h(State,Val):- get(State,[5,5],Elem1),get(State,[5,0],Elem2),Elem2 == 1 ,Elem1 == 1 ,countStones(State, 0, 0, P1, P2),Val is -500 + (P2 - P1).
h(State,Val):- get(State,[5,5],Elem1),get(State,[0,0],Elem2),Elem2 == 1 ,Elem1 == 1 ,countStones(State, 0, 0, P1, P2),Val is -600 + (P2 - P1).

h(State,Val):-get(State,[0,5],Elem1),Elem1 == 1 ,countStones(State, 0, 0, P1, P2),Val is 400 + (P2 - P1).

h(State,Val):-get(State,[5,0],Elem1),Elem1 == 1 ,countStones(State, 0, 0, P1, P2),Val is 400 + (P2 - P1).

h(State,Val):-get(State,[5,5],Elem1),Elem1 == 1 ,countStones(State, 0, 0, P1, P2),Val is 400 + (P2 - P1).
*/



h(State, Val) :-
 countStones(State, 0, 0, P1, P2), Val is (P2 - P1).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.

lowerBound(-1001).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.

upperBound(1001).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
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
% ?- initBoard(B),  showState(B),set(B, NB1, [2,4], 1),
%         set(NB1, NB2, [2,3], 1),  showState(NB2). 
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

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% get(Board, Point, Element): get the value of the board at position
% column X and row Y (indexing starts at 0).
% Do not change get:
get( Board, [X, Y], Value) :-
	nth0( Y, Board, ListY),
	nth0( X, ListY, Value).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% set( Board, NewBoard, [X, Y], Value): set the value of the board at position
% column X and row Y to Value (indexing starts at 0). Returns the new board as
% NewBoard. Do not change set:

set( [Row|RestRows], [NewRow|RestRows], [X, 0], Value)
    :- setInList(Row, NewRow, X, Value).

set( [Row|RestRows], [Row|NewRestRows], [X, Y], Value) :-
	Y > 0,
	Y1 is Y-1,
	set( RestRows, NewRestRows, [X, Y1], Value).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% setInList( List, NewList, Index, Value): given helper to set. Do not
% change setInList:

setInList( [_|RestList], [Value|RestList], 0, Value).

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :-
	Index > 0,
	Index1 is Index-1,
	setInList( RestList, NewRestList, Index1, Value).
