#!/usr/bin/env python3
"""List every changed snapshot case, without ignoring presentation fields."""
import collections, gzip, importlib.util, json
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4]
spec=importlib.util.spec_from_file_location('snapshot',ROOT/'dev/validation/fit-snapshot.py')
fs=importlib.util.module_from_spec(spec);spec.loader.exec_module(fs)
F=set('beta bnbinom cmb dlaplace dweibull fatigue frechet genbeta gompertz hcauchy huber invexp invparalogis invweibull llogis logitnorm naka paralogis pareto3 power sgomp sn st trgamma weibull'.split())
DEST=Path(__file__).resolve().parent
summary={}
for mode in ['native','double']:
 old=fs.load(ROOT/f'dev/research/evaluations/2026-11-14-fit-snapshot-data/{mode}.jsonl.gz')
 new=fs.load(ROOT/f'build/snapshots/{mode}')
 counts=collections.Counter();byfamily=collections.Counter();changed=[]
 for key in sorted(new):
  if key[0] not in F:continue
  a,b=old[key],new[key]
  assert a['input_sha256']==b['input_sha256'],key
  fields=[k for k in sorted(set(a['result'])|set(b['result'])) if a['result'].get(k)!=b['result'].get(k)]
  if not fields:continue
  ar,br=a['result'],b['result']
  if ar['kind']=='ok':
   assert br['kind']=='ok', ('refused old Ok',key)
   assert ar['estimates']==br['estimates'], ('changed Ok coefficients',key)
  if ar['kind']!=br['kind'] or ar.get('outcome')!=br.get('outcome'):
   category='outcome-meaning'
  elif fields==['logLik'] or set(fields)<=set(['logLik','limit_loglik','loglik_lower','loglik_upper','candidates']):
   category='represented-point-likelihood'
  elif set(fields)<=set(['message','core_status','outcome_override']):category='core-owned-rendering'
  elif key[0]=='st' and ('limit_estimates' in fields):category='restored-limit-endpoint'
  else:category='review-required'
  counts[category]+=1;byfamily[key[0]]+=1
  reasons={
   'outcome-meaning':'Core-owned input/collapse/identification classification; inspect the old and new outcomes and the per-family report entry.',
   'represented-point-likelihood':'Re-evaluate the reported likelihood at the returned binary64 coefficients, including harmless last-bit changes on ordinary data.',
   'core-owned-rendering':'Preserve or clarify the diagnostic after the core acquired the decision previously made by the adapter.',
   'restored-limit-endpoint':'Restore the exact observed half-law endpoint before evaluating its represented likelihood.',
   'review-required':'Requires explicit review before the report can be completed.'}
  assert category!='review-required',(key,fields)
  reason=reasons[category]
  if category=='outcome-meaning':
   reason={
    'bnbinom':('A fully held finite parameter point is trivially certified; there is no optimization.' if br['outcome']['unique'] else 'Exact size/beta symmetry and disjoint certified coefficient boxes prove two distinct global maximizers; both are attached.'),
    'cmb':('Size one attains the saturated Bernoulli likelihood for every finite nu; return finite family witnesses with the original nu=1 representative.' if key[1]=='archive' else 'At probability zero, dispersion is absent from the point-mass law; the existing representative is nonunique.'),
    'frechet':('Singleton scale/shape concentration proves unboundedness.' if br['reason']=='unbounded' else 'The two-point pair bound and its limiting sequence prove a finite unattained supremum.'),
    'pareto3':('The shape spike at the common positive excess proves unboundedness; shifted-data validity is decided before delegating.' if br['kind']=='nomle' else 'Nonfinite observations or observations outside the held support are invalid data, rather than invalid supplied arguments.'),
    'invparalogis':'The failed search returned a nonrepresentable coefficient or likelihood; the core now reports evaluation failure and carries the range diagnostic.',
    'sn':'A singleton is constant data: with free scale, the normal member concentrates there and proves unboundedness.',
    'st':'The observations are finite and valid; their standardization range overflows binary64, so the search fails in evaluation rather than input validation.',
    'trgamma':'A singleton is constant data; the generalized-gamma concentration path proves unboundedness.'
   }.get(key[0],reason)
  elif category=='represented-point-likelihood':
   reason={
    'hcauchy':'Sum the public half-Cauchy density at the returned mu and sigma, including the represented subnormal scale.',
    'weibull':'Sum the public Weibull density at the returned shape and scale; do not report the centered log-profile objective.',
    'st':'Evaluate the named normal or half-law at its restored binary64 coefficients and update its point witnesses.',
    'trgamma':'Evaluate the named lognormal or power law on the original observations at its returned coefficients; update point witnesses and widen the numerical upper bound by the objective discrepancy.'
   }.get(key[0],reason)
  elif category=='core-owned-rendering':
   reason='The adapter no longer intercepts empty or nonfinite data; preserve the core invalid-input outcome and use its family diagnostic.'
  changed.append(dict(family=key[0],source=key[1],case=key[2],category=category,reason=reason,fields=fields,old=ar,new=br))
 with gzip.open(DEST/f'{mode}-differences.jsonl.gz','wt') as out:
  for row in changed:out.write(json.dumps(row,sort_keys=True)+'\n')
 summary[mode]=dict(total=len(new),changed=len(changed),categories=dict(counts),families=dict(byfamily))
(DEST/'difference-counts.json').write_text(json.dumps(summary,indent=2)+'\n')
print(json.dumps(summary,indent=2))
