import numpy as np
import matplotlib.pyplot as plt
#defing the function 
def phi0(x):
    return np.where((x >= 40) & (x < 70), np.sin(np.pi * (x - 40) / 30)**2, 0)
#defining a function for the ftbs scheme 
def ftbs(phi_now):
    return (1.0 - c) * phi_now + c * np.roll(phi_now, 1)
#defining a function for the ftfs scheme
def ftfs(phi_now):
    return (1.0 + c) * phi_now - c * np.roll(phi_now, -1)

#inital conditions 
u = 0.087  
dx = 0.1  
dt = 1.1 
x0 = 0.0  
x1 = 100.0  
t0 = 0.0  
t1 = 1000.0  
tp = 200.0

c = u * dt / dx
nx = int(np.round((x1 - x0) / dx)) + 1
x = np.linspace(x0, x1, nx)
phi = np.vectorize(phi0)(x)

plt.figure(figsize=(10, 5))
t = t0
n = 0
while t < t1:
    if u > 0:
        phi_new = ftbs(phi)
    else:
        phi_new = ftfs(phi)
    
    phi = phi_new
    t += dt
    #plotting at every tp step
    if t % tp <= dt:
        plt.plot(x, phi, label=f'Time = {n}')
        n = n + tp
#displying the plot
plt.xlabel('x')
plt.ylabel('phi')
plt.title('Evolution of phi over time')
plt.legend()
plt.show()
