import numpy as np
import matplotlib.pyplot as plt
#defing the function 
def phi0(x):
    if x < 200.0:
        return 0.1
    elif x >= 200.0 and x < 250.0:
        return 2.0
    elif x >= 250.0 and x <= 300.0:
        return 1.0
    elif x > 300.0:
        return 0.1

#defining a function for the ftbs scheme 
def ftbs(phi_now):
    return (1.0 - c) * phi_now + c * np.roll(phi_now, 1)
#defining a function for the ftfs scheme
def ftfs(phi_now):
    return (1.0 + c) * phi_now - c * np.roll(phi_now, -1)

def ctcs(phi_old,phi_now):
    return phi_old - c * (np.roll(phi_now,-1) - np.roll(phi_now,1))

u = -0.31
dx = 0.1  
dt = 0.1 
x0 = 0.0  
x1 = 500.0  
t0 = 0.0  
t1 = 1000.0  
tp = 200
alpha = 0.05
beta = 0.53

c = u * dt / dx
nx = int(np.round((x1 - x0) / dx)) + 1
x = np.linspace(x0, x1, nx)
phi_old = np.vectorize(phi0)(x)
phi_now = phi_old
d = phi_old
if u > 0:
    phi_now = ftbs(phi_old)
else:   
    phi_now = ftfs(phi_old)
t = t0 + dt
n = 0
plt.figure(figsize=(10, 5))
plt.plot(x, phi_now, label=f'Time = {n}')
while t < t1:
    phi_new = ctcs(phi_old,phi_now)
    d = alpha * (phi_old + phi_new - 2.0 * phi_now)
    phi_old = phi_now + beta * d
    phi_now = phi_new + (1 - beta) * d
    t = t + dt
    if t % tp < dt:
        n = n + tp 
        plt.plot(x, phi_new, label=f'Time = {n}') 
plt.xlabel('x')
plt.ylabel('$\phi$')
plt.title('Evolution of $\phi$ over time')
plt.legend()
plt.show() 