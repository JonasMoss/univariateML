"""Independent 120-digit likelihoods at unchanged returned binary64 coefficients.
Run from the worktree after all four final snapshot modes have completed.
"""
import json,pathlib,mpmath as mp
mp.mp.dps=120
out=[]
def real(v):
 return mp.mpf(float.fromhex(v)) if isinstance(v,str) else mp.mpf(v)
def records(path):
 return (json.loads(line) for line in path.read_text().splitlines() if line.startswith('{'))
for mode in ['native','double']:
 for family,cases in [('gamma',['near-constant','offset']),('laplace',['subnormal']),('lgser',['huge'])]:
  for r in records(pathlib.Path(f'build/snapshots/{mode}/{family}.jsonl')):
   if r['source']!='class' or r['case'] not in cases:continue
   x=list(map(real,r['data']));p={k:real(v) for k,v in r['result']['estimates'].items()}
   if family=='gamma':
    a,b=p['shape'],p['scale'];ll=mp.fsum((a-1)*mp.log(v)-v/b-a*mp.log(b)-mp.loggamma(a) for v in x)
   elif family=='laplace':
    ll=mp.fsum(-mp.log(2*p['sigma'])-abs(v-p['mu'])/p['sigma'] for v in x)
   else:
    t=p['theta'];ll=mp.fsum(v*mp.log(t)-mp.log(v)-mp.log(-mp.log1p(-t)) for v in x)
   reported=real(r['result']['logLik'])
   out.append(dict(mode=mode,family=family,case=r['case'],reference=mp.nstr(ll,100),reported=float(reported),absolute_error=float(abs(ll-reported))))
 for ident in ['ztpois','zmpois']:
  for r in records(pathlib.Path(f'build/snapshots/routes-{mode}/{ident}.jsonl')):
   if r['source']!='class' or r['case']!='huge':continue
   x=list(map(real,r['data']));p={k:real(v) for k,v in r['result']['estimates'].items()}
   rate=p['lambda'];p0=p.get('p0',mp.mpf(0))
   ll=mp.fsum(-rate+v*mp.log(rate)-mp.loggamma(v+1)-mp.log1p(-mp.exp(-rate))+mp.log1p(-p0) for v in x)
   reported=real(r['result']['logLik'])
   out.append(dict(mode=mode,family=r['family'],case='huge',reference=mp.nstr(ll,100),reported=float(reported),absolute_error=float(abs(ll-reported))))
assert len(out)==12
assert all(r['absolute_error'] <= 1e-10*max(1,abs(r['reported'])) for r in out),out
print(json.dumps(out,indent=2))
