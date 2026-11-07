"""LP search for non-unimodal ZTNB profiles at fixed conditional means."""
import numpy as np,json,collections
from scipy.optimize import brentq,linprog
from pathlib import Path
from explore import param
out=[]; calls=0; statuses=collections.Counter()
x=np.arange(1,101); grids=np.logspace(-3,4,55)
for m in [1.1,1.5,2,3,5,10,20]:
 fs=[]
 for r in grids:
  t=param(r,m); J=1-t/np.expm1(t)
  fs.append(np.array([m*J-sum(j/(r+j) for j in range(1,int(y))) for y in x]))
 fs=np.array(fs)
 for i in range(0,len(grids)-2,2):
  for j in range(i+1,len(grids),3):
   # F at earlier size negative and at later size positive => minimum.
   A=np.zeros((2,101));A[0,:100]=fs[i];A[1,:100]=-fs[j];A[:,100]=1
   E=np.zeros((2,101));E[0,:100]=1;E[1,:100]=x
   res=linprog(np.r_[np.zeros(100),-1],A_ub=A,b_ub=[0,0],A_eq=E,b_eq=[1,m],bounds=[(0,None)]*100+[(None,None)],method='highs',options={'threads':1})
   calls+=1; statuses[str(res.status)]+=1
   if res.success and res.x[-1]>1e-8:
    row=dict(m=m,r1=grids[i],r2=grids[j],margin=res.x[-1],weights=[(int(v),float(w)) for v,w in zip(x,res.x[:100]) if w>1e-10]);out.append(row);print(row,flush=True);break
  if out and out[-1]['m']==m:break
Path(__file__).with_name('search-modes.json').write_text(json.dumps(dict(calls=calls,statuses=dict(statuses),results=out),indent=2)+'\n')
print('complete',calls,len(out),flush=True)
