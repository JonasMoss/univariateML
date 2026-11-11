"""Exact binary64 inputs, independent mpmath density and certified-root checks.
No C++ density, gradient, Hessian or moments enter the reference.
"""
import json,math,sys,collections
from pathlib import Path
import mpmath as mp
D=Path(__file__).resolve().parent;E=D.parent
sys.path.insert(0,str(E/'2026-11-11-acc-fb-t-data'))
from oracle import objective

def density(f,x,p):
 if f!='snorm':return objective(f,x,p)
 mu,s,xi=p;a=mp.sqrt(2/mp.pi)
 su=mp.sqrt((1-a*a)*(xi*xi+xi**-2)+2*a*a-1);shift=a*(xi-1/xi)
 def term(v):
  z=(v-mu)*su/s+shift;w=z/xi if z>0 else z*xi
  return -w*w/2
 return len(x)*(mp.log(su)-mp.log(s)+mp.log(2/(xi+1/xi))-mp.log(2*mp.pi)/2)+mp.fsum(term(v) for v in x)

def main(phase,mode):
 cases={c['index']:c for c in json.loads((D/'cases.json').read_text())}
 rows=list(map(json.loads,(D/f'{phase}-{mode}.jsonl').read_text().splitlines()))
 oldrefs={r['index']:r for r in map(json.loads,(E/'2026-11-11-acc-fb-t-data/stationary.jsonl').read_text().splitlines())}
 out=[];roots=[]
 for r in rows:
  c=cases[r['index']];f=c['family'];r=dict(r,family=f,sample=c['sample'],held=c['held'])
  if r['error']:out.append(r);continue
  mp.mp.dps=min(1600,max(110,int(4*max([abs(math.log10(abs(v))) for v in r['parameters'] if v]+[0]))+100))
  x=list(map(mp.mpf,c['data']));p=list(map(mp.mpf,r['parameters']));ref=density(f,x,p)
  r['precision_digits']=mp.mp.dps;r['reference']=str(ref)
  r['fitter_error']=float(abs(mp.mpf(r['logLik'])-ref)/max(1,abs(ref)))
  r['dpqr_error']=None if r['dpqr'] is None else float(abs(mp.mpf(r['dpqr'])-ref)/max(1,abs(ref)))
  r['seam_error']=None if r['dpqr'] is None else abs(r['logLik']-r['dpqr'])/max(1,abs(r['dpqr']))
  r['cusp']=False
  if f=='sged' and p[2]<1:
   a=mp.gamma(2/p[2])/mp.sqrt(mp.gamma(1/p[2])*mp.gamma(3/p[2]));su=mp.sqrt((1-a*a)*(p[3]**2+p[3]**-2)+2*a*a-1)
   radius=min(abs((v-p[0])*su/p[1]+a*(p[3]-1/p[3])) for v in x)
   r['cusp_radius']=str(radius);r['cusp']=radius<mp.mpf('1e-12')
  out.append(r)
  if 'reference_index' in c:
   target=oldrefs[c['reference_index']]['theta'];keys=list(target)
   # Repeat the independent stationary solve at 110 digits, in unit coordinates.
   origin,unit=mp.mpf(target['mean']),mp.mpf(target['sd']);y=[(v-origin)/unit for v in x]
   q=[(mp.mpf(target['mean'])-origin)/unit,mp.log(mp.mpf(target['sd'])/unit),mp.mpf(target['nu']),mp.mpf(target['xi'])]
   free=[j for j,k in enumerate(keys) if k not in c['held']]
   def fun(*z):
    t=q.copy()
    for j,v in zip(free,z):t[j]=v
    return density(f,y,[t[0],mp.exp(t[1]),t[2],t[3]])
   def score(*z):return tuple(mp.diff(fun,z,tuple(int(k==j) for k in range(len(z)))) for j in range(len(z)))
   z=tuple(q[j] for j in free)
   if free and q[2]>1:
    z=tuple(mp.findroot(score,z,tol=mp.mpf('1e-85'),maxsteps=30))
    for j,v in zip(free,z):q[j]=v
    theta=[origin+unit*q[0],unit*mp.exp(q[1]),q[2],q[3]]
   else:theta=[mp.mpf(target[k]) for k in keys]
   errors={keys[j]:float(abs(p[j]-theta[j])/max(abs(theta[j]),theta[1] if j==0 else 0)) for j in free}
   roots.append(dict(index=c['reference_index'],reference=[str(v) for v in theta],parameters=r['parameters'],errors=errors,maximum_scaled_error=max(errors.values(),default=0),certified=r['certified']))
 (D/f'{phase}-{mode}-checked.json').write_text(json.dumps(out,indent=2)+'\n')
 (D/f'{phase}-{mode}-roots.json').write_text(json.dumps(roots,indent=2)+'\n')
 print(phase,mode,'rows',len(rows),'success',sum(not r['error'] for r in rows))
 for f in ('sged','sstd','snorm'):
  v=[r for r in out if r['family']==f and not r['error']]
  print(f,'n',len(v),'fitter misses',[(r['index'],r['fitter_error'],r['cusp']) for r in v if r['fitter_error']>1e-10], 'dpqr misses',[(r['index'],r['dpqr_error']) for r in v if r['dpqr_error'] is None or r['dpqr_error']>1e-10])
 print('root max',max(r['maximum_scaled_error'] for r in roots))
if __name__=='__main__':main(*sys.argv[1:])
