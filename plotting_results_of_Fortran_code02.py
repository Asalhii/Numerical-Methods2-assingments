import matplotlib.pyplot as plt
#defining a function that reads and plots every 1001 line from a file
def read_and_plot(file_name="file.txt"):
    x_data = []
    phi_data = []
    plt.figure(figsize=(10, 6))
    #opening a file 
    with open(file_name, 'r') as file:
        lines = file.readlines()
        n = 0
        for i, line in enumerate(lines):
            #checking for empty lines and splitting the taken lines
            if line.strip():
                parts = line.split()
                #appending the lines to x and phi 
                try:
                    x = float(parts[0])
                    phi = float(parts[1])
                    x_data.append(x)
                    phi_data.append(phi)
                #skipping unreadable lines
                except ValueError:
                    continue
            #plotting at each 1001 line (each 200 step in this case)
            if (i + 1) % 1001 == 0:
                plt.plot(x_data, phi_data, label=f"$\phi$({n})")
                x_data.clear() 
                phi_data.clear()
                n = n + 200
    #displying the plot
    plt.xlabel("x")
    plt.ylabel("$\phi$(x)")
    plt.title("$\phi$(x) vs x at different time steps")
    plt.legend()
    plt.show()
#calling the function 
read_and_plot("advection.txt")

