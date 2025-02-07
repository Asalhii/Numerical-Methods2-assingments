import pandas as pd
import numpy as np
import matplotlib.pyplot as plt



data = pd.read_csv('integration.txt',skiprows=1, sep='\s+',header=None)
data = pd.DataFrame(data)

x = data[0]
y_euler = data[1]
y_heun = data[2]

plt.subplots(figsize=(10, 10))
plt.grid(color='grey', linestyle='-', linewidth=0.25, alpha=0.5)

plt.plot(x, y_euler,'b',label='Euler scheme')
plt.plot(x, y_heun,'r',label='Heun scheme')
plt.xlabel('x')
plt.ylabel('Euler, Heun')
plt.title('integral')
plt.legend()
plt.show()