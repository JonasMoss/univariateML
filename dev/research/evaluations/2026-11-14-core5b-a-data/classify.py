#!/usr/bin/env python3
"""Inventory every snapshot difference against master da2174a1's archived fit snapshots."""
from pathlib import Path
import sys,json,gzip,importlib.util,collections
ROOT=Path(__file__).resolve().parents[4]
spec=importlib.util.spec_from_file_location('snapshot',ROOT/'dev/validation/fit-snapshot.py')
module=importlib.util.module_from_spec(spec);spec.loader.exec_module(module)
F=set('bbinom bhatt cauchy dgamma dunif expweibull fpareto ged gev gumbel ht invburr invgauss invtrgamma lgamma logis lst nsbeta pareto2 pig sged slash sstd tlambda unif'.split())
base=ROOT/'dev/research/evaluations/2026-11-14-fit-snapshot-data'
output=Path(__file__).resolve().parent
reasons={
 'public-estimate-likelihood':'Sum the physical density at the unchanged returned coefficients.',
 'limit-payload':'Evaluate the named limit at its returned physical coefficients, including outward support rounding, and preserve the inherited certificate flag.',
 'named-boundary':'Replace the old boundary-as-failure rendering with the named, evaluated limit and its existing certification flag.',
 'core-collapse':'A valid degenerate sample has a proved nonexistence result, rather than an invalid-input or invalid-argument reason.',
 'core-nonexistence':'The core collapse status denotes nonexistence, rather than an old plain numerical failure.',
 'invalid-data-reason':'Data outside the held support are invalid data, rather than invalid held arguments.',
 'invalid-held-parameter-reason':'An invalid held parameter is an invalid argument, rather than invalid data.',
 'core-unsupported-holds':'The core explicitly records a structural unsupported hold configuration.',
 'unidentified-representative':'Return a representative and parameterization of the proved non-unique maximum set.',
 'nonunique-maximum':'The balanced Cauchy geometry proves non-uniqueness; retain every discrete maximizer or the full curve parameterization.',
 'finite-collapse-reason':'Exactly half the Cauchy sample at an atom with other support points gives finite nonattainment, rather than divergence.',
 'refuted-lognormal-face':'An independently checked feasible finite Feller-Pareto witness strictly exceeds the proposed lognormal face.',
 'unrepresentable-limit':'The proposed exponential rate exceeds binary64 range; return an evaluation failure instead of an infinite-rate payload.',
 'valid-data-arithmetic-failure':'A transformation or physical likelihood cannot be represented; the finite sample itself is valid.',
 'valid-small-sample-fit':'Use the existing analytic two-point GEV certificate instead of a blanket small-sample refusal.',
 'core-decision-and-diagnostics':'Carry the status and diagnostic in the core; the terminal outcome and estimate are unchanged.',
 'failed-search-reason':'The SSTD search produced no accepted proposal; this is non-convergence, not a mathematical nonexistence proof.',
 'unproved-scale-face':'The uncertified SSTD equality-collapse status denotes an unresolved comparison, rather than an optimizer convergence failure; no estimate or proved nonexistence is returned.',
 'failed-face-evaluation':'The SSTD search already failed; its attempted half-face has a nonfinite physical likelihood and is rejected as an evaluation failure, retaining only diagnostic coordinates.'
}
summary={}
for mode in sys.argv[1:] or ['native','double']:
 old=module.load(str(base/(mode+'.jsonl.gz')));new=module.load(str(ROOT/'build/snapshots'/mode))
 assert {k for k in old if k[0] in F}=={k for k in new if k[0] in F}, 'Snapshot case inventory changed'
 counts=collections.Counter();per_family=collections.Counter();rows=[];regressions=[]
 for key in sorted(new):
  if key[0] not in F:continue
  a=old[key]['result'];b=new[key]['result']
  af=dict(module.flatten(a));bf=dict(module.flatten(b))
  fields=[f for f in sorted(af.keys()|bf.keys()) if af.get(f)!=bf.get(f)]
  if not fields:continue
  if a['kind']=='ok' and b['kind']=='ok' and a['estimates']!=b['estimates']:
   regressions.append(key)
  changed={f.split('.')[0] for f in fields}
  if changed <= {'logLik'}:category='public-estimate-likelihood'
  elif a.get('outcome',{}).get('kind')=='limit' and b.get('outcome',{}).get('kind')=='limit':category='limit-payload'
  elif a.get('outcome',{}).get('reason') in ['invalid-input','invalid-argument'] and b.get('outcome',{}).get('kind')=='nonexistence':category='core-collapse'
  elif a.get('outcome',{}).get('reason')=='invalid-argument' and b.get('outcome',{}).get('reason')=='invalid-input':category='invalid-data-reason'
  elif b.get('outcome',{}).get('reason')=='unsupported':category='core-unsupported-holds'
  elif key[0]=='bbinom' and a.get('core_status')==6:category='unidentified-representative'
  elif a.get('outcome',{}).get('unique')!=b.get('outcome',{}).get('unique'):category='nonunique-maximum'
  elif key[0]=='cauchy' and b.get('outcome',{}).get('reason')=='unattained':category='finite-collapse-reason'
  elif a.get('outcome',{}).get('kind')=='failure' and b.get('outcome',{}).get('kind')=='limit':category='named-boundary'
  elif key[0]=='fpareto' and b.get('core_status')==18:category='refuted-lognormal-face'
  elif a.get('outcome',{}).get('reason')=='invalid-input' and b.get('outcome',{}).get('reason')=='invalid-argument':category='invalid-held-parameter-reason'
  elif a.get('outcome',{}).get('reason')=='invalid-input' and b.get('outcome',{}).get('kind')=='estimate':category='valid-small-sample-fit'
  elif a.get('outcome',{}).get('reason')=='invalid-input' and b.get('outcome',{}).get('reason')=='evaluation':category='valid-data-arithmetic-failure'
  elif b.get('outcome',{}).get('kind')=='nonexistence':category='core-nonexistence'
  elif key[0]=='pareto2' and b.get('outcome',{}).get('reason')=='evaluation':category='unrepresentable-limit'
  elif key[0]=='sstd' and a.get('outcome',{}).get('kind')=='failure' and b.get('core_status')==5 and b.get('outcome',{}).get('reason')=='unresolved':category='unproved-scale-face'
  elif key[0]=='sstd' and a.get('outcome',{}).get('kind')=='failure' and b.get('core_status')==12 and b.get('outcome',{}).get('reason')=='evaluation' and b.get('model')=='ht' and b.get('limit_loglik')=='nan':category='failed-face-evaluation'
  elif a.get('outcome')==b.get('outcome') and a.get('kind')==b.get('kind') and a.get('estimates')==b.get('estimates') and a.get('logLik')==b.get('logLik'):category='core-decision-and-diagnostics'
  elif key[0]=='sstd' and b.get('outcome',{}).get('reason')=='no-convergence':category='failed-search-reason'
  else:category='review'
  if category=='review':print('REVIEW',mode,key,json.dumps(dict(fields=fields,old=a,new=b),sort_keys=True))
  counts[category]+=1;per_family[key[0],category]+=1
  rows.append(dict(family=key[0],source=key[1],case=key[2],category=category,reason=reasons.get(category),fields=fields,old=a,new=b))
 assert not regressions, ('Existing Ok coefficients changed',regressions)
 assert 'review' not in counts, 'Unclassified snapshot differences remain'
 with gzip.open(output/(mode+'-differences.jsonl.gz'),'wt') as stream:
  for row in rows:stream.write(json.dumps(row,sort_keys=True)+'\n')
 summary[mode]=dict(total=len(new),changed=len(rows),categories=dict(counts),families={f:dict((c,n) for (ff,c),n in per_family.items() if ff==f) for f in sorted(F)},coefficient_regressions=regressions)
 print(mode,json.dumps(summary[mode],sort_keys=True))
(output/'difference-summary.json').write_text(json.dumps(summary,indent=2,sort_keys=True)+'\n')
