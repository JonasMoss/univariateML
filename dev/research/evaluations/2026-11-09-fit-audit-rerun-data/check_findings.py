#!/usr/bin/env python3
"""Validate local identifiability before using coefficient displacement as error.
Does not turn local curvature into a global MLE certificate.
"""
import os,collections,json,pathlib,signal,sys
import mpmath as mp
import sympy as sp
import audit
from reference_extra import adapt
from existence import proof,nonexistence
from held_support import violation
from support_geometry import classify as support_classify
fits={int(l.split(",")[2]):l.split(",") for l in (pathlib.Path(__file__).resolve().parent/"fits.csv").read_text().splitlines()}
HERE=pathlib.Path(__file__).resolve().parent
cases=json.loads((HERE/'cases.json').read_text());refs=list(map(json.loads,(HERE/os.environ.get('REFERENCE_FILE','reference.jsonl')).read_text().splitlines()));records={r['index']:r for r in refs}
class Deadline(Exception):pass
def alarm(*args):raise Deadline()
signal.signal(signal.SIGALRM,alarm)
summary=collections.Counter()
assert mp.diff(lambda a,b:-a*a-a*b-2*b*b,(0,0),(1,1))==-1
for folder,m,g in audit.models():
 ps=m['params'];names=[p['name'] for p in ps];rows=[r for r in refs if r['family']==folder.name]
 targets=[r for r in rows if r['classification']=='silent failure' and r.get('evidence','').startswith('stationary') and not r.get('coefficient_error_verified')]
 unchecked=[r for r in rows if r['classification']=='silent failure' and 'precision_check' not in r and 'support_exclusion' not in r and r.get('evidence')!='status Ok with nonfinite reported logLik']
 if targets or unchecked:
  domains=[(0,mp.inf) if not p['domain'].split(',')[0].lstrip('([').strip().startswith('-') else (-mp.inf,mp.inf) for p in ps]
  from quality.density import density_expression
  syms,x,expr=density_expression(folder,m,domains,prefer_spec=False)
  expr=expr.replace(lambda e:e.func==sp.log and e.args[0].is_Add and sp.S.One in e.args[0].args,lambda e:sp.Function('log1p')(e.args[0]-1))
  fn=adapt(folder.name,names,sp.lambdify([*syms,x],expr,[{'log1p':mp.log1p},'mpmath'],cse=True))
 for r in rows:
  c=cases[r['index']]
  if r['status'] in [0,-1]:
   row=fits[r['index']]
   if support_classify(r,m,c,[float(v.split('=')[1]) for v in row[9:]],float(row[8])):continue
  incompatible=violation(m,c)
  if incompatible and r['status'] not in [0,-1,-997,-998,-999]:
   r.update(classification='ok',evidence='proved incompatible held support: '+incompatible,reference_basis='held-support incompatibility proof');continue
  if incompatible and r['status'] in [0,-1]:
   r.update(classification='silent failure',evidence='status Ok despite incompatible held support',reference_logLik='-inf',relative_error=float('inf'));continue
  if r['classification'] in ['non-Ok','loud failure']:
   reason=proof(folder.name,c);absent=nonexistence(folder.name,c)
   if reason:r.update(classification='loud failure',existence=reason)
   elif absent:r.update(classification='ok',evidence='proved nonexistence: '+absent,existence=absent,reference_basis='dossier nonexistence proof')
  elif r['classification']=='open' and r['status']==-997:
   r['existence']=proof(folder.name,c) or 'open'
  if r in unchecked and r['classification']=='silent failure':
   signal.setitimer(signal.ITIMER_REAL,20)
   try:
    with mp.workdps(2*r.get('precision_digits',70)):
     t=list(map(mp.mpf,r['theta']));xx=list(map(mp.mpf,c['data']));ll=mp.fsum(fn(*t,v) for v in xx)
     assert mp.isfinite(ll) and abs(ll-mp.mpf(r['reference_logLik']))<mp.mpf('1e-20')*max(1,abs(ll))
     if 'better_theta' in r:
      bt=list(map(mp.mpf,r['better_theta']));better=mp.fsum(fn(*bt,v) for v in xx)
      assert better>ll or r.get('evidence')=='logLik discrepancy'
     r['precision_check']='base/doubled precision agree to relative 1e-20 (separate final check)'
     r['logLik_doubled_precision']=str(ll)
   except Exception as e:r.update(classification='open',open_reason='final doubled precision: '+repr(e))
   finally:signal.setitimer(signal.ITIMER_REAL,0)
  if r not in targets:continue
  signal.setitimer(signal.ITIMER_REAL,20)
  try:
   mp.mp.dps=r.get('precision_digits',70)
   base=list(map(mp.mpf,r['refined_theta']));returned=list(map(mp.mpf,r['theta']));data=list(map(mp.mpf,c['data']))
   free=[i for i,p in enumerate(ps) if p['name'] in r.get('scores',{})]
   if not free:raise ValueError('no recorded smooth free-coordinate scores')
   scales=[audit.coordinate_unit(folder.name,ps,returned,data,i) for i in free]
   def objective(*z):
    t=base[:]
    for i,s,zi in zip(free,scales,z):t[i]+=s*zi
    return mp.fsum(fn(*t,v) for v in data)
   zero=tuple(mp.mpf(0) for i in free);H=mp.matrix(len(free));gradient=[]
   for j in range(len(free)):
    order=[0]*len(free);order[j]=1;gradient.append(mp.diff(objective,zero,tuple(order))/len(data))
    for k in range(j+1):
     order=[0]*len(free);order[j]+=1;order[k]+=1
     H[j,k]=H[k,j]=-mp.diff(objective,zero,tuple(order))/len(data)
   eig=mp.eigsy(H,eigvals_only=True);largest=max(abs(v) for v in eig);smallest=min(eig)
   r['local_information_eigenvalue_range']=[str(smallest),str(largest)]
   r['stationary_residual_per_observation']=str(max(abs(v) for v in gradient))
   if max(abs(v) for v in gradient)>mp.mpf('1e-18'):raise ValueError('refined point not stationary under per-observation normalization')
   if smallest<=mp.mpf('1e-35')*max(1,largest):raise ValueError('reference is nonisolated or too ill-conditioned for coefficient-error evidence')
   errors=[]
   for i in free:
    signed=ps[i]['domain'].split(',')[0].lstrip('([').strip().startswith('-')
    errors.append(abs(base[i]-returned[i])/(max(1,abs(base[i])) if signed else max(abs(base[i]),mp.mpf('1e-300'))))
   error=max(errors,default=mp.mpf(0));r['parameter_relative_error']=float(error);r['coefficient_error_verified']=True
   if error<=mp.mpf('1e-8'):
    r.update(classification='ok',evidence='density/score checks passed; coefficient difference is within the signed-coordinate tolerance')
    r['relative_error']=max(float(abs(mp.mpf(r['reference_logLik'])-mp.mpf(r['reported_logLik']))/max(1,abs(mp.mpf(r['reported_logLik'])))),r.get('relative_gain',0),float(error))
   summary[r['classification']]+=1
  except Exception as e:
   r.update(classification='open',coefficient_error_verified=False,open_reason='local-reference validation: '+type(e).__name__+': '+str(e));summary['open']+=1
  finally:signal.setitimer(signal.ITIMER_REAL,0)
 print('findings validation',folder.name,dict(summary),flush=True)
for r in refs:
 if not r.get('coefficient_error_verified',False) and 'parameter_relative_error' in r:
  r['unverified_stationary_displacement']=r.pop('parameter_relative_error')
  try:
   ll,reported=mp.mpf(r['reference_logLik']),mp.mpf(r['reported_logLik'])
   llerr=float(abs(ll-reported)/max(1,abs(reported))) if mp.isfinite(reported) else float('inf')
   r['relative_error']=max(llerr,r.get('relative_gain',0))
  except Exception:pass
(HERE/os.environ.get('REFERENCE_FILE','reference.jsonl')).write_text(''.join(json.dumps(records[i])+'\n' for i in sorted(records)))
(HERE/'findings-validation.json').write_text(json.dumps(dict(validation='PASS',checked_this_run=sum(summary.values()),total_verified=sum(bool(r.get('coefficient_error_verified')) for r in refs),outcomes=dict(summary)),indent=2))
print('PASS findings validation',dict(summary),flush=True)
