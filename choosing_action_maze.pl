:-nl,write('Language Setting ...'),nl.

% type definitions
% pre-defined states
%:- dynamic state/1.
maxWidth(4).
maxHeight(4).

:-retractall(state(_)).

:-maxWidth(N),maxHeight(M),
  forall(between(0,N,X),(
  forall(between(0,M,Y),asserta(state((X,Y)))))).

:-writeln("The states have been inserted in database.").

% Actions
act(right).
act(left).
act(up).
act(down).

action(X):-act(X).

:-writeln('The actions (right,left,up,down) have been defined.').

:-writeln('Background Knowledge ...').

% Neighborhood
% adjacent(X,D,Y). means: if we be in state X and move in direction D, then we will be in the state Y.
adjacent((A,B),right,(C,D)):-state((A,B)),state((C,D)),D is B+1,C is A.
adjacent((A,B),left ,(C,D)):-state((A,B)),state((C,D)),D is B-1,C is A.
adjacent((A,B),up   ,(C,D)):-state((A,B)),state((C,D)),C is A-1,D is B.
adjacent((A,B),down ,(C,D)):-state((A,B)),state((C,D)),C is A+1,D is B.

%adjacent(X,right,Y):-adjacent(Y,left,X).
%adjacent(X,left,Y):-adjacent(Y,right,X).
%adjacent(X,up,Y):-adjacent(Y,down,X).
%adjacent(X,down,Y):-adjacent(Y,up,X).

:-writeln('The neighborhood relations have been defined.').

% wall(X) means: state X is an obstacle (wall).
wall((1,1)).
wall((2,2)).
wall((3,3)).
wall((4,0)).
wall((0,4)).

not_wall(X):-not(wall(X)).
:-writeln('The obstacles (walls) have been defined.'),nl.

remove_repeats(List, Result) :-
  setof(X, member(X, List), Result).
possible_actions(State,Actions):-state(State),findall(Action,(adjacent(State,Action,NextState),not_wall(NextState)),L),remove_repeats(L,Actions).