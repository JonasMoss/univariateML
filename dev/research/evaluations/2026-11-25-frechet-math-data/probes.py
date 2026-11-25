"""Reproduce the rational witness selection and the brackets used by replay."""
import numpy as np
from search import free, held, maxima

b = np.linspace(0,1,1001)
a = np.geomspace(1,50,1001)
for upper in (2.6, 2.7, 2.75, 2.77, 2.8, 2.9, 3):
    v=free(np.array([0.,1.,upper]),np.ones(3)/3,b)
    k=maxima(v)
    print('free',upper,b[k],v[k],v[0],
          'min',b[maxima(-v)], 'edge deltas',v[1]-v[0],v[-1]-v[-2])
for d, upper in ((.05,40),(.054,39),(.055,40),(.05,39),(.054,40),(.054,38),(.054,41)):
    v=held(np.array([0.,d,upper]),np.array([3,4,1])/8,a)
    k=maxima(v)
    print('held',d,upper,a[k],v[k], 'min',a[maxima(-v)])
for counts in [(1,2),(1,3),(1,4),(1,5),(1,6),(1,9),(4,6),(49,51)]:
    v=free(np.array([0.,1.]),np.array(counts)/sum(counts),np.array([0.,1.]))
    print('two',counts,v,v[1]-v[0])
for upper in (1,1.1,1.2,1.3,1.4,1.5,1.7,2):
    v=free(np.array([0.,1.,upper]),np.array([4,5,1])/10,b)
    k=maxima(v)
    print('face path',upper,b[k],v[-1]-v[0], (v[1]-v[0])*1000,
          v[500]-max(v[0],v[-1]))
