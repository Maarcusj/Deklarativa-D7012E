%Laboration P1
%Get packae

%Move room1
move(state(State,Skey,Bkey,Package),NewState,Move) :-
 State = r1,
 Skey = hand, 
 NewState = state(r2,Skey,Bkey,Package),
 Move = moveR2.
move(state(State,Skey,Bkey,Package),NewState,Move) :-
 State = r1,
 Bkey = hand,
 NewState = state(r3,Skey,Bkey,Package),
 Move = moveR3.

%Move room2
move(state(State,Skey,Bkey,Package),NewState,Move) :-
 State = r2,
 Skey = hand,
 NewState = state(r1,Skey,Bkey,Package),
 Move = moveR1.

%Move room3.
move(state(State,Skey,Bkey,Package),NewState,Move) :-
 State = r3,
 Bkey = hand,
 NewState = state(r1,Skey,Bkey,Package),
 Move = moveR1.

%Brasskey drop,take
move(state(State,Skey,Bkey,Package),NewState,Move) :-
 Bkey = hand,
 NewState = state(State,Skey,State,Package),
 Move = dropBkey.
move(state(State,Skey,Bkey,Package),NewState,Move) :-
 Bkey = State,
 NewState = state(State,Skey,hand,Package),
 Move = takeBkey.

%Steelkey drop,take
move(state(State,Skey,Bkey,Package),NewState,Move) :-
 Skey = hand,
 NewState = state(State,State,Bkey,Package),
 Move = dropSkey.
move(state(State,Skey,Bkey,Package),NewState,Move) :-
 Skey = State,
 NewState = state(State,hand,Bkey,Package),
 Move = takeSkey. 

%Package drop,take.
move(state(State,Skey,Bkey,Package),NewState,Move) :-
 Package = hand,
 NewState = state(State,Skey,Bkey,State),
 Move = dropPackage.
move(state(State,Skey,Bkey,Package),NewState,Move) :-
 Package = State,
 NewState = state(State,Skey,Bkey,hand),
 Move = takePackage.  

%state(room,hand(left,right))
solveR(state(_,_,_,r2),_,[]) :-
solveR(State,N,Trace) :-
 N > 0, 
 move(State,NewState,Move),
 \+(NewState = state(_,hand,hand,hand)),
 solveR(NewState,N-1,TraceCo),
 Trace = [Move|TraceCo].	
