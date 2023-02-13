# Symbolic-Q-Learning-using-Prolog-Programming

## Abstract

Q-learning is one the most powerful tabular algorithms that can tackle acute, optimization problems using Bellman equations. However, this algorithm has some disadvantages such as lack of safety when choosing an action. In fact, when this method takes an actions from the action space, it does not make sure what the consequense of the chosen actions is and tries to take actions blindly. In this project, we aim to solve this problem using symbolic logic programming. Indeed, in each state, we try to remove those actions which have a negative conqesuence using Prolog Programming and PySwip interface. By doing so, we bring the safety of the agent to the next level. Implementing the simulation in Python, we show that the symbolic Q-learning method can receive bigger rewards as compared to conventional Q-learing method.

## Maze Environment

![image](https://github.com/98210184/Symbolic-Reinforcement-Learning-Prolog-Programming/blob/main/maze5x5.png)

### Prolog Language Settings
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


#### Connecting Python to Prolog

In this section, we make use of [PySwip](https://pypi.org/project/pyswip/), as an interface between Python and Prolog, to import `choosing_action_maze.pl` file into python using the following commands:

```
from pyswip import Prolog

prolog = Prolog()
prolog.consult('choosing_action_maze.pl')
```

#### Import Safe actions to Python
```
def safe_actions(state):
    X, Y = state[0], state[1]
    input = "safe_actions(("+str(X)+","+str(Y)+"),Actions)"
    L = list(prolog.query(input))
    
    Actions = []
    for action in L[0]['Actions']:
        Actions.append(str(action))
    return Actions
```

### Symbolic Q-Learning
```
for episode in range(num_episodes):
    state = (0, 0) # Start from a start state
    done = False
    episode_reward = 0

    for step in range(max_steps_per_episode):
        
        if sq == 1:
            actions = safe_actions(state)
            print(f"State: {state}, Possible Actions: {actions}. \n")

        if random.uniform(0, 1) < exploration_rate:
            action = random.choice(actions)
        else:
            act = np.argmax(list(q_table[(state, a)] for a in actions))
            action = actions[act]

        next_state = NextState(state, action)
        print(f"Chosen Action: {action}, Next State: {next_state}. \n")

        reward = maze[next_state[0]][next_state[1]]
        episode_reward += reward

        q_value = q_table[(state, action)]
        max_q_value = max(q_table[(next_state, a)] for a in actions)
        new_q_value = (1 - learning_rate) * q_value + learning_rate * (reward + discount_rate * max_q_value)
        q_table[(state, action)] = new_q_value

        state = next_state

        if reward == 10:
            done = True
            break

    cumulative_reward.append(episode_reward)
    exploration_rate = min_exploration_rate + (max_exploration_rate - min_exploration_rate) * np.exp(-exploration_decay_rate * episode)
    print(f"Episode number {episode}, Cumulative Reward: {episode_reward}. ================================|| \n\n")
```

#### comparison of the rewards received by Q-Learning and Symbolic Q-Learning

[image](https://github.com/98210184/Symbolic-Reinforcement-Learning-Prolog-Programming/blob/main/data/rewards-comparison.png)
