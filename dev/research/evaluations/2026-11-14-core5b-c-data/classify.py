#!/usr/bin/env python3
"""Reproduce the complete per-case snapshot delta inventory (no fits)."""
import collections,gzip,importlib.util,json,sys,types
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4]
sys.path.insert(0,str(ROOT/'dev/validation'))
sp=importlib.util.spec_from_file_location('fs',ROOT/'dev/validation/fit-snapshot.py');fs=importlib.util.module_from_spec(sp);sp.loader.exec_module(fs)
OUT=Path(__file__).resolve().parent
F='betapr burr cmp dnorm emg foldnorm gb2 gengamma gpd hnorm hyper invgamma invpareto kumar lnorm lomax nhyper pareto pareto4 rayleigh skellam snorm std triang yulesimon'.split()
summary={};changes=[]
for mode in ['native','double']:
 provenance=json.load(open(ROOT/f'build/snapshots/{mode}/provenance.json'))
 current=fs.fingerprint(types.SimpleNamespace(no_long_double=mode=='double',cpu_limit=300))
 assert provenance['source_sha256']==current['source_sha256'],(mode,'snapshot sources changed; rerun')
 (OUT/f'{mode}-provenance.json').write_text(json.dumps(provenance,indent=2)+'\n')
 before=fs.load(ROOT/f'dev/research/evaluations/2026-11-14-fit-snapshot-data/{mode}.jsonl.gz')
 after=fs.load(ROOT/f'build/snapshots/{mode}')
 counts=collections.Counter();families=collections.Counter();unchanged=0
 for key,r in sorted(after.items()):
  if key[0] not in F:continue
  a,b=before[key]['result'],r['result']
  if a==b:unchanged+=1;continue
  keys=[k for k in sorted(set(a)|set(b)) if a.get(k)!=b.get(k)]
  if a['kind']=='ok':
   assert b['kind']=='ok',(mode,key,a,b)
   assert a['estimates']==b['estimates'],(mode,key,'Ok coefficients changed')
  if a.get('outcome')!=b.get('outcome') or a['kind']!=b['kind']:category='outcome-correction'
  elif keys==['core_status']:category='core-status-only'
  elif set(keys)<= {'logLik','limit_loglik','loglik_lower','loglik_upper','candidates'}:category='public-coefficient-likelihood'
  elif key[0] in ['hnorm','std','snorm','burr','pareto4']:category='likelihood-and-stop-payload'
  elif key[0]=='emg':category='EMG-accurate-comparison-candidates'
  else:category='stop-payload-and-diagnostic'
  reason={
    'betapr':'Core Unbounded status replaces adapter constant-data reclassification.',
    'burr':'Singleton nonexistence or Pareto/Weibull face likelihood evaluated after coefficient rounding.',
    'cmp':'Fully-held evaluation policy moved unchanged into the core.',
    'emg':'Remove binding payload suppression; refine finite comparison candidates before declaring accuracy.',
    'gb2':'Constant-data nonexistence; named limit payload; exterior-held-scale simultaneous-escape endpoint face comparison.',
    'gpd':'Free-threshold nonexistence or an unsupported held slice is decided in the core.',
    'hnorm':'Empty-data reason in core; likelihood evaluated at public rounded scale.',
    'hyper':'Support incompatibility is invalid input, rather than unattained supremum; no binding override.',
    'kumar':'Unbounded likelihood means nonexistence.',
    'lomax':'Unrepresentable exponential rate is evaluation failure with no invalid limit payload.',
    'nhyper':'Infeasible support is invalid input, rather than unattained supremum.',
    'pareto':'Held endpoint incompatibility classified in core.',
    'pareto4':'Burr singleton/face correction; core support checks and free-threshold nonexistence/unsupported decisions.',
    'rayleigh':'Zero likelihood has invalid-input reason; evaluate likelihood at returned scale.',
    'snorm':'Evaluate half-normal face at public rounded scale; remove binding payload suppression.',
    'std':'Evaluate normal face at public rounded coefficients.',
  }.get(key[0],'Remove binding stop suppression and outcome override; core outcome is authoritative.')
  if key[0]=='gb2':
   if b.get('reason')=='evaluation' and a.get('reason')=='invalid-input':reason='Rounded logarithmic equality of distinct observations is an arithmetic failure, not constant-data nonexistence.'
   elif b.get('reason')=='unbounded':reason='Core proves constant-data nonexistence and returns Unbounded instead of InvalidData.'
   elif b.get('model') in ['pareto','power'] and 'scale' in r['held'] and b.get('limit_estimates') and float.fromhex(b['limit_estimates']['b' if b['model']=='pareto' else 'alpha'])!=r['held']['scale']:
    reason='The simultaneous power/shape escape allows a free endpoint despite held scale; return the dominating Pareto/power point and payload, uncertified.'
   elif b.get('model')=='lnorm':reason='LognormalBoundary now carries its model, public coefficients and point likelihood, as an uncertified limit.'
   elif 'double Pareto' in a.get('message','') or 'double-pareto' in (b.get('model') or ''):
    reason='DoubleParetoBoundary now carries its profiled model (or one-sided endpoint face), public coefficients and point likelihood, as an uncertified limit.'
  elif key[0]=='gpd' and b.get('reason')=='unattained':
   reason='Finite threshold-scale collapse has an unattained supremum, not an evaluable named limiting model; retain its supremum diagnostic.'
  elif key[0]=='burr':
   reason=('Core proves singleton nonexistence rather than rejecting valid input.' if b.get('reason')=='unbounded' else
           'Recompute the '+(b.get('model') or a.get('model') or 'selected')+' face likelihood after exporting its physical coefficients; preserve global certificate bounds.')
  elif key[0]=='hnorm':
   reason=('Core distinguishes empty input from scale collapse.' if key[2]=='empty' else
           'Evaluate the likelihood at the unchanged rounded subnormal scale, without the exact-optimum n/2 substitution.')
  elif key[0]=='rayleigh':
   reason=('An identically zero likelihood is an invalid-input failure, not an unattained positive supremum.' if b.get('reason')=='invalid-input' else
           'Evaluate the likelihood at the unchanged rounded subnormal scale, without the exact-optimum n substitution.')
  elif key[0]=='pareto4':
   if b.get('reason')=='evaluation' and a.get('reason')=='invalid-input':reason='Rounded logarithmic equality of distinct observations is an arithmetic failure, not constant-data nonexistence.'
   elif b.get('reason')=='unbounded':reason='The core proves singleton/free-threshold nonexistence instead of an input or unsupported-fit refusal.'
   else:
    parts=[]
    if any(k in keys for k in ['limit_loglik','loglik_lower','loglik_upper','candidates']):parts.append('evaluate the Burr face at its exported physical coefficients')
    if 'message' in keys:parts.append('correct the parent-fitter advice for a held endpoint or translated data')
    if 'model' in keys or 'limit_estimates' in keys:parts.append('carry the held translation in the limit model and coefficients')
    reason='; '.join(parts) or reason
  elif key[0]=='snorm':
   reason=('Core records an unrepresentable half-normal face as evaluation failure; no binding payload veto remains.' if b.get('reason')=='evaluation' else
           'Use the original sample extreme for the free half-normal endpoint and evaluate the boundary likelihood in public coordinates.')
  counts[category]+=1;families[key[0]]+=1
  changes.append(dict(mode=mode,family=key[0],source=key[1],case=key[2],category=category,reason=reason,fields=keys,before=a,after=b))
 summary[mode]=dict(cases=sum(1 for k in after if k[0] in F),unchanged=unchanged,changed=sum(counts.values()),categories=dict(counts),families=dict(families),ok_coefficients_changed=0)
 with gzip.open(OUT/f'{mode}.jsonl.gz','wt') as dst:
  for k,r in sorted(after.items()):
   if k[0] in F:dst.write(json.dumps(r,sort_keys=True)+'\n')
(OUT/'snapshot-summary.json').write_text(json.dumps(summary,indent=2)+'\n')
with gzip.open(OUT/'snapshot-changes.json.gz','wt') as dst:json.dump(changes,dst,sort_keys=True)
(OUT/'snapshot-changes.tsv').write_text('mode\tfamily\tsource\tcase\tcategory\tfields\treason\n'+''.join('\t'.join([c['mode'],c['family'],c['source'],c['case'],c['category'],','.join(c['fields']),c['reason']])+'\n' for c in changes))
print(json.dumps(summary,indent=2))
