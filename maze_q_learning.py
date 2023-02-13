import os
import numpy as np
import random
import matplotlib.pyplot as plt
from pyswip import Prolog
import pandas as pd

# Q-learning {sq=0} and Symbolic Q-learning {sq=1}
sq = 1

print(os.getcwd())

# Prolog initialization
prolog = Prolog()
prolog.consult('choosing_action_maze.pl')

# create dataframe
df = pd.DataFrame()

def safe_actions(state):
    X, Y = state[0], state[1]
    input = "safe_actions(("+str(X)+","+str(Y)+"),Actions)"
    L = list(prolog.query(input))

    Actions = []
    for action in L[0]['Actions']:
        Actions.append(str(action))

    #print(f"State: {state}, Possible Actions: {Actions}. \n")
    return Actions

def NextState(state, action):

    if action == 'right':
        next_state = (state[0], min(state[1] + 1, n-1))       
    elif action == 'left':
        next_state = (state[0], max(state[1] - 1, 0))       
    elif action == 'up':
        next_state = (max(state[0] - 1, 0), state[1])
    elif action == 'down':
        next_state = (min(state[0] + 1, n-1), state[1])

    return next_state

# Define the environment
n = 5
maze = -np.ones((n, n))
maze[1][1] = -100 # Add an obstacle
maze[2][2] = -100
maze[3][3] = -100
maze[0][4] = -100
maze[4][0] = -100
maze[4][4] = 10 # Set the goal state
states = [(i, j) for i in range(n) for j in range(n)]
actions = ['right', 'left', 'up', 'down']

# Define the Q-table
q_table = {}
for state in states:
    for action in actions:
        q_table[(state, action)] = 0.0

# Define the parameters
num_episodes = 1000
max_steps_per_episode = 100
learning_rate = 0.1
discount_rate = 0.95
exploration_rate = 0.1
max_exploration_rate = 1.0
min_exploration_rate = 0.01
exploration_decay_rate = 0.001

# Store cumulative reward per episode
cumulative_reward = []

# The Q-Learning algorithm
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

df['reward'] = cumulative_reward

# Plot the cumulative reward per episode
plt.plot(cumulative_reward)
plt.xlabel('Episode')
plt.ylabel('Cumulative Reward')

if sq == 0:
    plt.savefig('img/Reward_Q_learning')
    df.to_csv('data/q_rewards.csv')

if sq == 1:
    plt.savefig('img/Reward_Symbolic_Q_learning')
    df.to_csv('data/symbolic_q_rewards.csv')


#plt.show()

