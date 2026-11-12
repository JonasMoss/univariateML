"""Assign a reviewable reason to every retirement replay difference; fail on unknowns."""
from pathlib import Path
from load_evidence import read_json
import json,csv,collections
p=Path(__file__).resolve().parent
rows=read_json(p/'results.json');cases=read_json(p/'cases.json')
changes=[];unknown=[]
for r in rows:
 if not r['differences']:continue
 c=cases[r['case']-1];a=c['args'];reason=None;category=None
 if r['family']=='zipf':
  ns=(r['new_estimates'] or {}).get('s');os=(r['old_estimates'] or {}).get('s')
  if a.get('N')=='NA':
   category='estimated window bounds';reason='NA requests an estimated upper window bound; the old explicit held-NA N coefficient was rejected.'
  elif isinstance(a.get('N'),(float,int)) and a['N']%1:
   category='closed-window rounding';reason='The closed modifier window uses floor(N); the old N coefficient required an integer.'
  elif isinstance(a.get('s'),(float,int)) and a['s']<0:
   category='widened exponent domain';reason='Finite zeta windows admit every finite real exponent; old Zipf rejected held negative s.'
  elif ns is not None and os==0 and ns<0:
   category='widened exponent domain';reason='The old nonnegative-shape maximum was the uniform boundary; the real-domain maximum has negative s and higher likelihood.'
   assert r['logLik_change']>=-1e-10
  elif r['new_kind']=='limit' and r['old_status']=='Ok' and os==0:
   category='widened exponent domain';reason='All observations at the upper endpoint have an unattained supremum as s tends to minus infinity; the old constrained maximum was uniform.'
  elif r['old_status']==r['new_status']=='Ok' and r['differences']=='coefficients':
   category='zeta accuracy refinement';reason='The new monotone-score bracket refines the old Newton stopping tolerance; independent high-precision score evidence is recorded separately.'
 elif r['family'] in ('tbinom','tpois','tnorm'):
  if r['family']=='tnorm' and all(a.get(k) is not None for k in ('mean','sd')) and r['old_kind']=='numerical' and r['new_status']=='Ok':
   assert all(r['new_estimates'][k]==a[k] for k in ('mean','sd'))
   category='held-window density improvement';reason='Both parent coefficients are held. Estimated bounds are the observed extremes; direct conditional-density evaluation avoids an unnecessary failed Newton solve. A 250-digit narrow-window oracle checks this row.'
  elif r['family']=='tbinom' and a.get('size')==1e308 and r['old_kind']=='numerical' and r['new_status']=='Ok':
   target=sum(c['x'])/len(c['x'])/a['size']
   assert abs(r['new_estimates']['prob']/target-1)<1e-10
   category='held-size numerical improvement';reason='The held-size modifier uses a bracketed exponential-family solver, resolving the full-window size=1e308 case where the old normalizer search failed; its probability agrees with the closed-form mean(x)/size within 1e-10 relative error.'
  elif any(a.get(k)=='NA' for k in ('a','b')):
   category='estimated window bounds';reason='NA requests an estimated modifier bound; old explicit held-NA inputs were rejected.'
  elif r['family']=='tpois' and 'a' not in a:
   category='estimated window bounds';reason='The modifier receives explicit NA bounds and estimates them; the old wrapper required a design argument.'
  elif 'must be in (-inf, inf)' in r['old_message']:
   category='unbounded window accepted';reason='The old fully held metadata path rejected infinite design bounds; the modifier accepts a proper half-infinite or unbounded window.'
  else:
   x=c.get('x',[]);aa=a.get('a');bb=a.get('b')
   if bb=='Inf' and (aa==0 or aa is None) and x and min(x)>=1:
    category='positive-window extension';reason='The [1,Inf] window aliases the zero-truncated model, including its attained prob/lambda=0 extension and singleton representatives; the conditional law and attained likelihood agree where both return estimates.'
    if r['old_status']==r['new_status']=='Ok':assert abs(r['logLik_change'])<=1e-10
 if reason is None:unknown.append(r);category='UNCLASSIFIED';reason='Requires investigation.'
 changes.append(dict(case=r['case'],family=r['family'],source=r['source'],differences=r['differences'],category=category,reason=reason))
with (p/'difference-reasons.csv').open('w') as f:
 w=csv.DictWriter(f,fieldnames=['case','family','source','differences','category','reason']);w.writeheader();w.writerows(changes)
summary=dict(cases=len(rows),differences=len(changes),unclassified=len(unknown),categories=dict(collections.Counter(r['category'] for r in changes)),families={family:dict(cases=sum(r['family']==family for r in rows),differences=sum(r['family']==family for r in changes),sources=dict(collections.Counter(r['source'].split('-')[0] for r in rows if r['family']==family))) for family in sorted(set(r['family'] for r in rows))})
(p/'audit-summary.json').write_text(json.dumps(summary,indent=2)+'\n');print(json.dumps(summary,indent=2))
assert not unknown,unknown
