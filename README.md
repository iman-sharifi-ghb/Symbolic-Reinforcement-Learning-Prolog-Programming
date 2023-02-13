# Symbolic-Q-Learning-using-Prolog-Programming

## Abstract


## Maze Environment


# State Transition in Maze using Aleph

![image](https://github.com/98210184/Symbolic-Reinforcement-Learning-Prolog-Programming/blob/main/maze5x5.png)

## Language Settings
#### Mode Declaration and Background Knowledge

#### States

`state(X).` means, X is a state.

```
maxWidth(4).
maxHeight(4).
:-retractall(state(_)).

:-maxWidth(N),maxHeight(M),
  forall(between(0,N,X),(
  forall(between(0,M,Y),asserta(state((X,Y)))))).
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
adjacent((A,B),right,(C,D)):-state((A,B)),state((C,D)),D is B+1,C is A.
adjacent((A,B),left ,(C,D)):-state((A,B)),state((C,D)),D is B-1,C is A.
adjacent((A,B),up   ,(C,D)):-state((A,B)),state((C,D)),C is A-1,D is B.
adjacent((A,B),down ,(C,D)):-state((A,B)),state((C,D)),C is A+1,D is B.
```

#### Obstacles (walls)

`wall(X).` means, state X is an obstacle (wall).
```
wall((1,1)).
wall((2,2)).
wall((3,3)).
wall((4,0)).
wall((0,4)).

not_wall(X):-not(wall(X)).
```

#### Finding safe actions in each state

Dangerous actions, the actions by taking which, the agent will strike the wall, will be removed in each state.
```
remove_repeats(List, Result) :-
  setof(X, member(X, List), Result).
  
safe_actions(State,Actions):-state(State),findall(Action,(adjacent(State,Action,NextState),not_wall(NextState)),L),remove_repeats(L,Actions).
```
