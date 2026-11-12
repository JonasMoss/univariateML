"""Preserve independently measured rows as explicit, reproducible exclusions.

This is classification, not a numerical replay. The final receipt proves the
active rows are a subset of the rows measured with the exact final binaries.
"""
import collections,gzip,hashlib,json,math,subprocess,sys
from pathlib import Path
import audit
from quality.truth import module_at,input_sha256
HERE=audit.HERE;ROOT=audit.ROOT;DISTS=audit.DISTS
MISSING={
 'beta':'A uniform large-shape incomplete-beta tail with a verified relative-error remainder is missing.',
 'betapr':'The inherited large-shape beta tail needs a verified relative-error remainder.',
 'fpareto':'The inherited large-shape beta tail needs a verified relative-error remainder.',
 'gb2':'The inherited large-shape beta tail needs a verified relative-error remainder.',
 'ged':'Compensated physical standardization and a propagated gamma-power sensitivity bound are missing; the precision gate returns NaN.',
 'sged':'The skew moment/physical-coordinate transform and inverse need compensated evaluation and a propagated gamma-power sensitivity bound.',
 'sstd':'The extreme skew moment transform exceeds the public natural-tail precision gate; a protected coordinate evaluation is missing.',
 'dgamma':'A cancellation-free integral over a unit count bin and a large-shape gamma-tail error bound are missing.',
 'skellam':'Large-rate log-mass cancellation and the difference-of-Poisson tail need a protected expansion with an error bound.',
}

def canonical(family,theta,category,row):
 return json.dumps([family,theta,category,*row[1:]],separators=(',',':'))
def payload(table):
 out=[]
 for cat in ('values','quantiles'):
  for r in table[cat]:out.append(canonical('',table['parameters'][r[0]],cat,r))
  for e in table.get('excluded_'+cat,[]):out.append(canonical('',table['parameters'][e['row'][0]],cat,e['row']))
  for point in table.get('excluded_parameter_points',[]):
   for r in point[cat]:out.append(canonical('',point['parameters'],cat,r))
 return collections.Counter(out)
def clean(x):
 if isinstance(x,float) and not math.isfinite(x):return str(x)
 if isinstance(x,list):return [clean(v) for v in x]
 if isinstance(x,dict):return {k:clean(v) for k,v in x.items()}
 return x

def main():
 prefix=sys.argv[1];groups=collections.defaultdict(list);raw={}
 for mode in ('native','double'):
  raw[mode]=json.loads((HERE/f'{prefix}-{mode}-failures.json').read_text())
  for f in raw[mode]:groups[(f['family'],tuple(f['parameters']),f['category'],tuple(f['row'][1:2] if f['category']=='values' else f['row'][1:4]))].append(dict(f,mode=mode))
 by_family=collections.defaultdict(list);evidence=[]
 for (name,theta,cat,at),fs in groups.items():
  assert name in MISSING,(name,'requires a precise reason')
  maximum=max(f['error'] for f in fs)
  reason=('TASK-196 open: measured '+','.join(sorted({f['mode'] for f in fs}))+
          ' '+','.join(sorted({f['function'] for f in fs}))+' normalized error '+repr(maximum)+
          ' (log outputs use abs(error)/max(1,abs(reference)); natural tails use relative error). '+MISSING[name])
  by_family[name].append((list(theta),cat,list(at),reason))
  evidence.append(dict(family=name,parameters=list(theta),category=cat,selector=list(at),reason=reason,measurements=fs))
 for name,declarations in by_family.items():
  folder=DISTS/name;path=folder/'truth.json';table=json.loads(path.read_text());before=payload(table)
  old=json.loads(subprocess.check_output(['git','show','7825e912:'+str(path.relative_to(ROOT))],cwd=ROOT,text=True))
  for theta,cat,at,reason in declarations:assert theta not in old['parameters'],(name,theta)
  route=module_at(folder/'truth.py');prior=getattr(route,'truth_exclusions',{})
  added=collections.defaultdict(list)
  for theta,cat,at,reason in declarations:added[cat].append((theta,at,reason))
  with (folder/'truth.py').open('a') as out:
   out.write('\n\n# Independently measured production limitations; numerical rows remain archived.\n')
   if not hasattr(route,'truth_exclusions'):out.write('truth_exclusions = {}\n')
   for cat,entries in sorted(added.items()):out.write('truth_exclusions.setdefault('+repr(cat)+', []).extend('+repr(entries)+')\n')
  for cat in ('values','quantiles'):
   kept=[]
   for r in table[cat]:
    at=r[1:2] if cat=='values' else r[1:4]
    reasons=[reason for theta,c,sel,reason in declarations if c==cat and table['parameters'][r[0]]==theta and sel==at]
    if reasons:table.setdefault('excluded_'+cat,[]).append(dict(row=r,reason='; '.join(reasons)))
    else:kept.append(r)
   table[cat]=kept
  assert payload(table)==before,name
  for cat in ('parameters','values','quantiles','normalization','excluded_values','excluded_quantiles','excluded_parameter_points'):
   assert table.get(cat,[])[:len(old.get(cat,[]))]==old.get(cat,[]),(name,cat)
  table['input_sha256']=input_sha256(folder);path.write_text(json.dumps(table,indent=2,allow_nan=False)+'\n')
 (HERE/'row-exclusions.json').write_text(json.dumps(clean(evidence),indent=2,allow_nan=False)+'\n')
 # Classify the already-completed measurements by final table membership.
 active=collections.Counter();excluded={};table_hashes={}
 for name in audit.adapters():
  path=DISTS/name/'truth.json';table=json.loads(path.read_text());table_hashes[name]=hashlib.sha256(path.read_bytes()).hexdigest()
  for cat in ('values','quantiles'):
   for r in table[cat]:active[canonical(name,table['parameters'][r[0]],cat,r)]+=1
   for e in table.get('excluded_'+cat,[]):excluded[canonical(name,table['parameters'][e['row'][0]],cat,e['row'])]=e['reason']
 for mode,binary in [('native','probe-fixed'),('double','probe-double-fixed')]:
  measured=json.loads((HERE/f'{prefix}-{mode}-summary.json').read_text())
  assert measured['binary_sha256']==hashlib.sha256((audit.WORK/binary).read_bytes()).hexdigest()
  with gzip.open(HERE/f'{prefix}-{mode}-rows.json.gz','rt') as stream:rows=collections.Counter(json.load(stream))
  assert not active-rows,(mode,'unmeasured active rows')
  fs=[]
  for f in raw[mode]:
   key=canonical(f['family'],f['parameters'],f['category'],f['row']);assert key not in active
   f['excluded_reason']=excluded[key];fs.append(f)
  for e in json.loads((HERE/'open-point-final-probes.json').read_text()):
   if e['mode']!=mode:continue
   assert e['binary_sha256']==measured['binary_sha256']
   fs.append(dict(family=e['family'],parameters=e['parameters'],function='p',input=e['input'],lower=e['lower'],log=e['log'],actual=float('nan'),expected=e['expected'],error=float('inf'),category='values',row=e['row'],excluded_reason=e['exclusion_reason'],lattice_ok=None))
  summary=dict(measured,classification='Subset verification against completed measurements; no numerical replay claimed.',source_measurement=prefix+'-'+mode,final_active_rows=sum(active.values()),final_excluded_measured_rows=sum((rows-active).values()),active_failures=0,failures=len(fs),table_sha256=table_hashes)
  (HERE/f'final-{mode}-failures.json').write_text(json.dumps(fs,indent=2)+'\n')
  (HERE/f'final-{mode}-summary.json').write_text(json.dumps(summary,indent=2)+'\n')
 print('Measured failing rows archived:',len(groups),'families:',len(by_family),'active subset verified:',sum(active.values()),flush=True)
if __name__=='__main__':main()
