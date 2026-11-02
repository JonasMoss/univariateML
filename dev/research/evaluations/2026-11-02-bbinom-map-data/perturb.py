import os
for k in ('OPENBLAS_NUM_THREADS','OMP_NUM_THREADS','MKL_NUM_THREADS'): os.environ[k]='1'
import numpy as np
import map
rng=np.random.default_rng(3113)
best=[]
for i in range(240):
 h=int(rng.choice([5,8,12,20])); mu=float(rng.choice([1,2,3,5])); x=rng.poisson(mu,h)
 if not x.max(): continue
 y=x.copy(); y[int(rng.integers(h))]+=1
 a=map.analyse(x,70); b=map.analyse(y,70)
 def finite(r): return r['grid_type']=='interior' and r['grid']>max(r['nb'],r['binom'],r['poisson'])+1e-5
 if finite(a)!=finite(b) or (finite(a) and finite(b) and abs(a['grid_N']-b['grid_N'])>=3):
  print('CASE',a,b,flush=True)
