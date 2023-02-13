# Symbolic-Q-Learning-using-Prolog-Programming

## Abstract


## Maze Environment


# State Transition in Maze using Aleph

![image](https://github.com/98210184/Symbolic-Reinforcement-Learning-Prolog-Programming/blob/main/maze5x5.png)

## Language Settings
#### Mode Declaration and Determination
```
:-modeh(*,next_state(+state,+act,-state)).
:-modeb(*,adjacent(+state,+act,-state)).
:-modeb(*,not_wall(+state)).
:-modeb(*,wall(+state)).

:-determination(next_state/3,adjacent/3).
:-determination(next_state/3,not_wall/1).
:-determination(next_state/3,wall/1).
```

## Background Knowledge

#### States

`state(X).` means, X is a state.
```
state((1,1)). state((2,1)). state((3,1)).
state((1,2)). state((2,2)). state((3,2)).
state((1,3)). state((2,3)). state((3,3)).
```

If you want to creats maze states automatically, please use this code block:
```
:- dynamic state/1.
maxWidth(3).
maxHeight(3).

create:-
	maxWidth(N),
	maxHeight(M),
	forall(between(1,N,X),(
	forall(between(1,M,Y),assertz(state((X,Y)))))).
```

#### Actions

`action(X).` means, X is an action.
```
act(right).
act(left).
act(up).
act(down).

action(X):-act(X).
```

#### Neighborhood (adjacent)

`adjacent(X,D,Y).` means, if we be in state X and move in direction D, then we will be in the state Y.
```
adjacent((A,B),right,(C,D)):-state((A,B)),state((C,D)),D is B,C is A+1,!.
adjacent((A,B),left ,(C,D)):-state((A,B)),state((C,D)),D is B,C is A-1,!.
adjacent((A,B),up   ,(C,D)):-state((A,B)),state((C,D)),C is A,D is B-1,!.
adjacent((A,B),down ,(C,D)):-state((A,B)),state((C,D)),C is A,D is B+1,!.

adjacent(X,right,Y):-adjacent(Y,left,X).
adjacent(X,up,Y):-adjacent(Y,down,X).
```

#### Obstacles (walls)

`wall(X).` means, state X is an obstacle (wall).
```
wall((1,1)).
wall((1,2)).
wall((3,2)).
wall((3,3)).

not_wall(X):-not(wall(X)).
```
