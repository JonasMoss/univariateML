"""Independent 70-digit score roots on the exact binary64 input samples.
No production jets, profile optimizers or interval code are used.
"""
import json,sys,time
from pathlib import Path
import mpmath as mp
mp.mp.dps=70
def exact(x):
    a,b=float(x).as_integer_ratio();return mp.mpf(a)/b
def scores(x,mu,s,xi):
    g=[mp.mpf(0)]*3
    for v in x:
        z=(v-mu)/s;t=1+xi*z
        L=mp.log1p(xi*z)/xi if xi else z
        A=1+xi-mp.exp(-L)
        Lx=(xi*z/t-mp.log1p(xi*z))/xi**2 if xi else -z*z/2
        g[0]+=A/(s*t);g[1]+=-1+A*z/t;g[2]+=-L-A*Lx
    return g
out=[]
for filename in sys.argv[1:]:
 for line in Path(filename).read_text().splitlines():
    row=json.loads(line.replace(":nan",":NaN").replace("[nan","[NaN").replace(",nan",",NaN"))
    if row['status']!=0 or (row['certified'] and not row.get('check_accuracy',False)):continue
    started=time.perf_counter();x=list(map(exact,row['data']));c=list(map(exact,row['coef']))
    if row['family']=='gev':
      mu,s,xi=c;free=[i for i in range(3) if (i!=0 or row['hold']!='mu') and (i!=1 or row['hold']!='sigma') and (i!=2 or xi not in (-1,1))]
      # Reverse-exponential corner coefficients are analytic, including the
      # inactive-support conditional scale optimum for a location hold.
      if xi==-1:
        ss=max(max(x)-mu,mu-sum(x)/len(x)) if row['hold']=='mu' else s
        root=[mu if row['hold']=='mu' else max(x)-ss,ss,xi]
      else:
        def build(z):
          r=[mu,s,xi]
          for k,v in zip(free,z):r[k]=mp.exp(v) if k==1 else v
          return r
        def f(*z):
          m,b,k=build(z);g=scores(x,m,b,k);return tuple(g[j] for j in free)
        init=tuple(mp.log(c[j]) if j==1 else c[j] for j in free)
        z=mp.findroot(f,init,tol=mp.mpf('1e-60'),maxsteps=50)
        root=build(z)
      floors=[root[1],root[1],mp.mpf(1)]
    elif row["hold"]=="none":
      lam,mu,s=c;k=1/lam;b=s*k;m=mu+s
      def f(m,l,k):return tuple(scores(x,m,mp.exp(l),k))
      z=mp.findroot(f,(m,mp.log(b),k),tol=mp.mpf("1e-60"),maxsteps=50)
      b=mp.exp(z[1]);root=[1/z[2],z[0]-b/z[2],b/z[2]];floors=[0,root[2],0]
    else:
      lam,mu,s=c
      def f(l,u):
        a=mp.exp(l);m=-mp.exp(u)
        logs=[mp.log((v-m)/s) for v in x];ys=[mp.exp(-a*v) for v in logs]
        gl=sum(1/a-L+L*y for L,y in zip(logs,ys))
        gm=sum((a+1-a*y)/(v-m) for v,y in zip(x,ys))
        return gl,gm
      z=mp.findroot(f,(mp.log(lam),mp.log(-mu)),tol=mp.mpf('1e-60'),maxsteps=50)
      root=[mp.exp(z[0]),-mp.exp(z[1]),s];floors=[0,s,s]
    errors=[abs(a-b)/max(abs(b),fl) for a,b,fl in zip(c,root,floors)]
    result={k:row[k] for k in ('sample_id','family','hold','status','certified')}
    result.update(source=Path(filename).name,root=[mp.nstr(v,65) for v in root],errors=[float(e) for e in errors],max_error=float(max(errors)),seconds=time.perf_counter()-started)
    out.append(result);print(json.dumps(result),flush=True)
    if max(errors)>mp.mpf('1e-10'):raise SystemExit('ACCURACY CONTRACT FAILED')
