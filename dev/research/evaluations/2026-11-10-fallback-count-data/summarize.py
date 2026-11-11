#!/usr/bin/env python3
"""Validate the per-fit flag/held contract and summarize the frozen study."""
import collections,json,math,pathlib
HERE=pathlib.Path(__file__).resolve().parent
cases=json.loads((HERE/'cases.json').read_text())
def read(name):
 out={}
 for line in (HERE/name).read_text().splitlines():
  a=line.split('\t');theta={}
  for field in a[4:]:
   if '=' in field and field.split('=',1)[0] in {'size','alpha','beta','m','N','k','r','prob','pi','p0','a','b'}:
    k,v=field.split('=',1);theta[k]=float(v)
  out[int(a[0])]=dict(family=a[1],outcome=a[2],logLik=float(a[3]),estimates=theta)
 return out
before,after=read('before-final.tsv'),read('after-final.tsv')
assert set(before)==set(after)==set(range(len(cases)))
summary={};violations=[];changes=[];roundoff=[];likelihood_roundoff=[];previous_successes=0;transitions=collections.Counter();routes=collections.defaultdict(collections.Counter)
for i,c in enumerate(cases):
 a,b=before[i],after[i];f=c['family'];transitions[(f,a['outcome'],b['outcome'])]+=1
 if c['source']!='2026-11-09 audit replay':routes[c['sample']][b['outcome']]+=1
 if b['outcome'] in ['certified','uncertified']:
  if not math.isfinite(b['logLik']) or not all(math.isfinite(v) or (k in ['a','b'] and math.isinf(v)) for k,v in b['estimates'].items()):violations.append([i,'nonfinite success'])
  for k,v in c['held'].items():
   if b['estimates'].get(k)!=v:violations.append([i,'held value changed',k,v,b['estimates'].get(k)])
 if a['outcome']=='certified' and b['outcome'] in ['certified','uncertified']:
  previous_successes+=1
  for k,v in a['estimates'].items():
   w=b['estimates'][k]
   if v!=w:
    error=abs(v-w)/abs(v) if v else math.inf
    roundoff.append(dict(index=i,family=f,coefficient=k,before=v,after=w,relative_difference=error))
  if a['logLik']!=b['logLik']:
   likelihood_roundoff.append(dict(index=i,family=f,before=a['logLik'],after=b['logLik'],scaled_difference=abs(a['logLik']-b['logLik'])/(1+abs(a['logLik']))))
  delta={k:[v,b['estimates'].get(k)] for k,v in a['estimates'].items() if abs(v-b['estimates'].get(k,v))>1e-10*max(1,abs(v))}
  if delta:changes.append(dict(index=i,family=f,sample=c['sample'],parameters=delta,old_logLik=a['logLik'],new_logLik=b['logLik']))
for f in sorted({c['family'] for c in cases}):
 summary[f]={label:dict(collections.Counter(v['outcome'] for v in data.values() if v['family']==f)) for label,data in [('before',before),('after',after)]}
result=dict(seed=1681,n=len(cases),outcomes=summary,transitions=[dict(family=f,before=a,after=b,n=n) for (f,a,b),n in sorted(transitions.items())],route_outcomes={k:dict(v) for k,v in routes.items()},contract_violations=violations,changed_previously_successful_estimates=changes)
(HERE/'summary.json').write_text(json.dumps(result,indent=2,allow_nan=False))
preservation=dict(previous_successes=previous_successes,changed_coefficients=roundoff,violations_at_relative_1e10=[r for r in roundoff if r['relative_difference']>1e-10],max_relative_difference=max((r['relative_difference'] for r in roundoff),default=0),logLik_changes=likelihood_roundoff,max_scaled_logLik_difference=max((r['scaled_difference'] for r in likelihood_roundoff),default=0))
assert not preservation['violations_at_relative_1e10']
assert preservation['max_scaled_logLik_difference']<1e-10
(HERE/'estimate-preservation.json').write_text(json.dumps(preservation,indent=2,allow_nan=False)+'\n')
print(json.dumps(summary,indent=2));print('contract violations',len(violations),'changed successful cases',len(changes))
assert not violations
print('previous successes',previous_successes,'coefficient roundoff changes',len(roundoff),'likelihood roundoff changes',len(likelihood_roundoff),'max scaled likelihood difference',preservation['max_scaled_logLik_difference'])
