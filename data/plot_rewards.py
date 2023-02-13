import pandas as pd
import matplotlib.pyplot as plt

df1 = pd.read_csv('q_rewards.csv')
df2 = pd.read_csv('symbolic_q_rewards.csv')

rewards1 = df1['reward']
rewards2 = df2['reward']

plt.figure(figsize=(10,7))
plt.plot(rewards1, label='q-learning')
plt.plot(rewards2, label='symblic q-learning')
plt.title('Reward')
plt.xlabel('Episode')
plt.ylabel('Cumulative reward')
plt.legend()
plt.savefig('rewards-comparison')