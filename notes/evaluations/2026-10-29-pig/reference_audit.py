"""Reproduce the actuar audit (existing R, mpmath, numpy, matplotlib).
Run from the repo root under /tmp/umvl-cpu/run. Outputs stay beside this file.
No replacement R density is defined.
"""
from pathlib import Path
import subprocess, csv, os, tempfile
os.environ.setdefault("MPLCONFIGDIR", tempfile.mkdtemp(prefix="pig-matplotlib-"))
import mpmath as mp
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
folder=Path(__file__).resolve().parent
mus=[.01,.1,1,2,10,100,500,750,1000,2000,10000]
ratios=np.arange(-8.,25.)
rows=[]
for mu in mus:
    for exponent in ratios:
        shape=mu**2*10**exponent
        for k in sorted(set([0,2,int(round(mu))])):
            rows.append((k,mu,shape,exponent))
script='library(actuar)\n'
script+='x <- matrix(c('+','.join(format(v,'.17g') for row in rows for v in row[:3])+'),ncol=3,byrow=TRUE)\n'
script+='cat(as.character(packageVersion("actuar")),"\\n",file=stderr()); cat("UMVL_VALUES_BEGIN\\n"); write.table(dpoisinvgauss(x[,1],x[,2],x[,3],log=TRUE),row.names=FALSE,col.names=FALSE); cat("UMVL_VALUES_END\\n")\n'
ref=subprocess.run(['Rscript','-'],input=script,capture_output=True,text=True,check=True)
print(ref.stderr.strip(),flush=True)
payload=ref.stdout.split('UMVL_VALUES_BEGIN',1)[1].split('UMVL_VALUES_END',1)[0]
values=[float(s) for s in payload.split()]
assert len(values)==len(rows)
mp.mp.dps=100
out=[]
for (k,mu,shape,e),val in zip(rows,values):
    m,l=mp.mpf(str(mu)),mp.mpf(str(shape))
    a=2+l/m**2;z=mp.sqrt(l*a)
    # Exact K_(k-1/2) polynomial. All terms are positive; unlike the
    # general hypergeometric Bessel implementation this converges at
    # every grid point, including very high order and argument.
    n=max(0,k-1); term=mp.mpf(1); poly=term
    for j in range(1,n+1):
        term *= mp.mpf((n+j)*(n-j+1))/(2*z*j)
        poly += term
    s=mp.sqrt(1+2*m*m/l)
    exact=-2*m/(1+s)+k*mp.log(m/s)-mp.loggamma(k+1)+mp.log(poly)
    out.append(dict(k=k,mean=mu,shape=shape,log10_shape_over_mean2=e,actuar_logp=val,mpmath_logp=float(exact),absolute_log_error=abs(val-float(exact))))
with (folder/'reference-grid.csv').open('w') as f:
    w=csv.DictWriter(f,fieldnames=out[0]);w.writeheader();w.writerows(out)
fig,ax=plt.subplots(figsize=(10,4))
mat=np.zeros((len(mus),len(ratios)))
for row in out:
    if row['k']==round(row['mean']):
        err=row['absolute_log_error']
        mat[mus.index(row['mean']),list(ratios).index(row['log10_shape_over_mean2'])]=3 if not np.isfinite(err) else 2 if err>1e-3 else 1 if err>1e-8 else 0
from matplotlib.colors import ListedColormap,BoundaryNorm
im=ax.imshow(mat,origin='lower',aspect='auto',extent=[ratios[0]-.5,ratios[-1]+.5,-.5,len(mus)-.5],cmap=ListedColormap(['#e2eadb','#f4d781','#db795e','#5d385a']),norm=BoundaryNorm([-.5,.5,1.5,2.5,3.5],4))
ax.set_yticks(range(len(mus)),labels=[str(m) for m in mus]);ax.set_ylabel('Mean (count = rounded mean)');ax.set_xlabel('log10(shape / mean²)')
cb=fig.colorbar(im,ax=ax,ticks=[0,1,2,3]);cb.ax.set_yticklabels(['≤ 1e-8','1e-8 to 1e-3','> 1e-3','nonfinite'])
ax.set_title('actuar log-pmf error against 100-digit Bessel-K evaluation')
fig.tight_layout();fig.savefig(folder/'reference-region.png',dpi=160)
print('Rows:',len(out),'nonfinite:',sum(not np.isfinite(r['absolute_log_error']) for r in out),'error >1e-8:',sum(r['absolute_log_error']>1e-8 for r in out))
