import numpy as np
import matplotlib.pyplot as plt
#defining diffrent functions 
def F(x,y):
    r = -0.5 * y + 4.0 * np.exp(-0.5 * x) * np.cos(4.0 * x)
    return r
def Fs(x):
    rs = np.exp(-0.5 * x) * np.sin(4.0 * x)
    return rs
def euler(x,y,dx):
    re = y + dx * F(x,y)
    return re
def heun(x,y,dx):
    f0 = F(x,y)
    y_star = y + dx * f0
    xp = x + dx
    rh = y + 0.5 * dx * (f0 + F(xp,y_star))
    return rh
#intial conditions
x0 = 0.0
x1 = 100
y0 = 0.0
dx = 0.1
y_eu = y0
y_he = y0
x = x0
xval, Euler_sol, Heun_sol = [], [], []
xval.append(x0)
Euler_sol.append(y_eu)
Heun_sol.append(y_he)
#calculating the results in a loop
for i in range(0,x1):
    y_eu = euler(x,y_eu,dx)
    y_he = heun(x,y_he,dx)
    x += dx
    y_sol = Fs(x)
    xval.append(x)
    Euler_sol.append(y_eu - y_sol)
    Heun_sol.append(y_he - y_sol)
#plotting the results 
plt.subplots(figsize=(10, 10))
plt.grid(color='grey', linestyle='-', linewidth=0.25, alpha=0.5)

plt.plot(xval, Euler_sol,'b',label='Euler scheme')
plt.plot(xval, Heun_sol,'r',label='Heun scheme')
plt.xlabel('x')
plt.ylabel('Euler, Heun')
plt.title('integral')
plt.legend()
plt.show()