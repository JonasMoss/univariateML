"""Numerical witnesses only: fixed-alpha modes and diagonal comparison."""
import json
from pathlib import Path
import numpy as np
from scipy.special import gammaln
from scipy.optimize import minimize, minimize_scalar
p=Path('univariateML-cpp/include/cppunivariate/distributions/bnbinom/golden.json')
g=json.loads(p.read_text()); x=np.array(g['data']); a=g['estimates']['alpha']
values,counts=np.unique(x,return_counts=True)
def ll(r,b):
    return np.dot(counts,gammaln(r+values)-gammaln(r)-gammaln(values+1)+gammaln(a+r)+gammaln(b+values)-gammaln(a+r+b+values)-gammaln(a)-gammaln(b)+gammaln(a+b))
def objective(u):
    if np.any(np.abs(u)>20):return 1e100
    return -ll(*np.exp(u))
rows=[]
for r in [.1,1,6,40]:
    for b in [.1,1,6,40]:
        f=minimize(objective,np.log([r,b]),method='BFGS',options={'gtol':1e-7,'maxiter':600})
        rows.append(dict(start=[r,b],coef=list(np.exp(f.x)),logLik=-float(f.fun),success=bool(f.success)))
grid=np.linspace(-10,10,401)
diag=[]
for l,h in zip(grid[:-1],grid[1:]):
    f=minimize_scalar(lambda u:-ll(np.exp(u),np.exp(u)),bounds=(l,h),method='bounded')
    diag.append((float(f.fun),float(np.exp(f.x))))
f,s=min(diag)
print(json.dumps(dict(alpha=a,off_diagonal=rows,diagonal=dict(shape=s,logLik=-f,log_shape_range=[-10,10],certificate=False)),indent=2))
