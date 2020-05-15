import numpy as np 
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit
from matplotlib import rc
#rc('text', usetex=True)


def func(x, a, b, c):
    return a**(b*x+c)

def l_func(x, a, b):
    return a*x + b

data = np.loadtxt('tower_of_hanoi.rend')

n = np.array(range(data.shape[0]+1))
 
n = np.delete(n, 0) 

popt, pcov = curve_fit(func, n, data[:,0])
#print(popt)

data = np.loadtxt('buffons_needle.eff')

N = np.loadtxt('calculated_pi.bn')

#print(data[:,1])
RAM = np.polyfit(n, data[:,1], 1)
#print(*RAM.shape)
#print(*popt)

xFit = np.arange(0.0, 25.0, 0.01)

fig, ax1= plt.subplots()

ax1.plot(n, data[:,0], '.--k', label='t de cómputo')
ax1.plot(xFit, func(xFit, *popt), '-r', label=r'$t(n) = %5.1f^{%5.1f n %5.1f}$' % tuple(popt))
plt.grid('--k',linewidth=0.5)
ax1.legend(['Tiempo medido',r'$t(n) = %5.1f^{%5.1f n %5.1f}$' % tuple(popt)], loc=9)
ax1.set_ylabel('Tiempo [s]')
ax1.set_xlabel('Número de discos n')
#ax1.savefig('t_of_hanoi_t_eff.pdf')

ax2 = ax1.twinx()
ax2.plot(n, data[:,1], '.--k', label='RAM utilizada')
ax2.plot(n, l_func(n, *RAM), '-b')
ax2.set_ylabel('Memoria RAM [Mb]')
ax2.legend(['RAM utilizada',r'$RAM(n) = %5.1f n + %5.1f}$' % tuple(RAM)], loc=1)

#h1, l1 = ax1.get_legend_handles_labels()
#h2, l2 = ax2.get_legend_handles_labels()
#ax1.legend(h1+h2, l1+l2, loc=9)

fig.tight_layout()
plt.savefig('t_of_hanoi_t_eff.pdf')
#plt.show()

