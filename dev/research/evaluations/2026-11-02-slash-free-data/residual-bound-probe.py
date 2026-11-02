import numpy as np
from scipy.optimize import brentq, minimize_scalar
import heapq
x=np.array([-3.,-1.,0.,2.,8.]); n=len(x)
def prof(d):
    def score(z):
        t=np.minimum((d*np.exp(-z))**2/2, 700)
        a=np.ones(n); mask=t<40
        a[mask]=np.where(t[mask]<1e-7,-1+t[mask],1-2*t[mask]/np.expm1(np.maximum(t[mask],1e-300)))
        return a.sum()
    z=brentq(score,-30,10,xtol=1e-12)
    t=np.maximum((d*np.exp(-z))**2/2,1e-300)
    return (-.5*np.log(2*np.pi)-z+np.log(-np.expm1(-t))-np.log(2*t)).sum()
def bound(a,b): return prof(np.maximum(np.maximum(a-x,x-b),0))
r=minimize_scalar(lambda m:-prof(abs(x-m)),bounds=(-1,2),method='bounded',options={'xatol':1e-12}); best=-r.fun
q=[]
for a,b in zip(x[:-1],x[1:]): heapq.heappush(q,(-bound(a,b),a,b))
for count in range(200000):
    u,a,b=heapq.heappop(q)
    if -u<=best+1e-9*(1+abs(best)):
        print('numerical stopping condition',count,'best',best);break
    m=(a+b)/2
    for c,d in ((a,m),(m,b)): heapq.heappush(q,(-bound(c,d),c,d))
else: print('budget',count+1,'remaining gap',-q[0][0]-best,'queue',len(q))
