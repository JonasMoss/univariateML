"""Independent arbitrary-precision fixed-shape/skew roots for every certified replay.
Mode is solved by monotone bisection, not the production log-score kernel.
Held-mean scale uses direct differentiated density in an independent coordinate.
"""
import json,sys,math
from pathlib import Path
import mpmath as m
D=Path(__file__).resolve().parent
sys.path.insert(0,str(D.parent/'2026-11-11-acc-fb-t-data'))
from oracle import objective,corner_reference
m.mp.dps=110

def run():
 cs={c['index']:c for c in json.loads((D/'cases.json').read_text())}
 before={r['index']:r for r in map(json.loads,(D/'before-native.jsonl').read_text().splitlines())}
 after={r['index']:r for r in map(json.loads,(D/'after-native.jsonl').read_text().splitlines())}
 targets={}
 for i,old in before.items():
  b=old if not old['error'] else after[i]
  c=cs[i];h=c['held']
  if c['family']!='sged' or b['error'] or b['certified']!=1:continue
  x=list(map(m.mpf,c['data']));p=list(map(m.mpf,b['parameters']))
  if 'nu' in h and 'mean' in h and 'sd' in h and 'xi' not in h:
   mu,sd,nu=m.mpf(h['mean']),m.mpf(h['sd']),m.mpf(h['nu'])
   y=[(v-mu)/sd for v in x]
   fun=lambda z:objective('sged',y,[m.mpf(0),m.mpf(1),nu,m.exp(z)])
   z=m.findroot(lambda z:m.diff(fun,z),m.log(p[3]),tol=m.mpf('1e-90'))
   assert -m.diff(fun,z,2)>0 and abs(m.diff(fun,z))<m.mpf('1e-80')
   targets[i]=[mu,sd,nu,m.exp(z)];continue
  if 'nu' not in h and 'xi' in h and p[2]==1:
   ref=corner_reference(c,p,x);assert ref is not None and ref['active_shape']
   targets[i]=[m.mpf(ref['theta'][k]) for k in ('mean','sd','nu','xi')];continue
  if not ('nu' in h and 'xi' in h):raise AssertionError(('unreferenced certified route',i,h))
  nu,xi=m.mpf(h['nu']),m.mpf(h['xi'])
  a=m.gamma(2/nu)/m.sqrt(m.gamma(1/nu)*m.gamma(3/nu));shift=a*(xi-1/xi)
  su=m.sqrt((1-a*a)*(xi*xi+xi**-2)+2*a*a-1);lam=m.sqrt(m.gamma(1/nu)/m.gamma(3/nu))
  if 'mean' not in h:
   origin=(min(x)+max(x))/2;unit=max(x)-min(x)
   if not unit:mode=x[0]
   else:
    y=[(v-origin)/unit for v in x]
    def cost(c):return m.fsum((abs(v-c)*(xi if v<c else 1/xi))**nu for v in y)
    if nu>1:
     l,r=min(y),max(y)
     for _ in range(370):
      t=(l+r)/2;s=m.fsum((xi**nu if t>v else -xi**-nu)*abs(t-v)**(nu-1) for v in y if t!=v)
      if s>0:r=t
      else:l=t
     knot=(l+r)/2
    else:knot=min(y,key=cost)
    mode=origin+unit*knot
   sd=m.mpf(h['sd']) if 'sd' in h else su/lam*(nu*m.fsum((abs(v-mode)*(xi if v<mode else 1/xi))**nu for v in x)/len(x))**(1/nu)
   mean=mode+sd*shift/su
  else:
   mean=m.mpf(h['mean'])
   if 'sd' in h:sd=m.mpf(h['sd'])
   elif xi==1:sd=su/lam*(nu*m.fsum(abs(v-mean)**nu for v in x)/len(x))**(1/nu)
   else:
    # Normalize by the fitted scale for the independent scalar score.
    unit=p[1];y=[(v-mean)/unit for v in x]
    if nu<1:
     knots=[-(v-mean)*su/shift for v in x if -(v-mean)*su/shift>0]
     near=min(knots,key=lambda v:abs(m.log(v/p[1]))) if knots else None
    else:near=None
    if near is not None and abs(m.log(near/p[1]))<m.mpf('1e-8'):sd=near
    else:
     fun=lambda z:objective('sged',y,[m.mpf(0),m.exp(z),nu,xi])
     z=m.findroot(lambda z:m.diff(fun,z),(-m.mpf('.0001'),m.mpf('.0001')),tol=m.mpf('1e-90'))
     assert abs(m.diff(fun,z))<m.mpf('1e-80')
     sd=unit*m.exp(z)
  targets[i]=[mean,sd,nu,xi]
 summary={};details=[]
 for phase in ('before','after'):
  for mode in ('native','double'):
   fn=D/f'{phase}-{mode}.jsonl'
   if not fn.exists():continue
   fits={r['index']:r for r in map(json.loads,fn.read_text().splitlines())}
   group=[]
   for i,t in targets.items():
    r=fits[i];c=cs[i]
    row=dict(index=i,family='sged',sample=c['sample'],phase=phase,mode=mode,certified=r['certified'],error=r['error'],reference=[str(v) for v in t])
    if not r['error']:
     row['components']={k:float(abs(m.mpf(r['parameters'][j])-t[j])/max(abs(t[j]),t[1] if j==0 else 0)) for j,k in enumerate(('mean','sd','nu','xi')) if k not in c['held']}
     row['maximum_scaled_error']=max(row['components'].values(),default=0)
    group.append(row);details.append(row)
   summary[phase+'-'+mode]=dict(rows=len(group),stops=[r['index'] for r in group if r['error']],misses=[r['index'] for r in group if not r['error'] and r['maximum_scaled_error']>1e-10],maximum=max(r['maximum_scaled_error'] for r in group if not r['error']))
 (D/'all-certified-roots.json').write_text(json.dumps(dict(summary=summary,rows=details),indent=2)+'\n')
 print(json.dumps(summary,indent=2))
if __name__=='__main__':run()
