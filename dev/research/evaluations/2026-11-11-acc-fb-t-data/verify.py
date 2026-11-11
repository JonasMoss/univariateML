"""Acceptance checks, distinguishing owned searches, preserved certificates,
and the unchanged external GED transport. No numeric tolerances are widened."""
from pathlib import Path
import json,csv,math,mpmath as m
D=Path(__file__).resolve().parent;m.mp.dps=90
cases={r['index']:r for r in json.loads((D/'cases.json').read_text())}
# All 462 formerly uncertified Student rows, with the exact stored samples.
source=D.parent/'2026-11-08-std-free-data'
seeded=list(csv.DictReader((source/'seeded-coverage.csv').open()))
expected={(r['family'],r['source']+'-'+r['seed'],int(r['held_mask'])) for r in seeded if r['certified']=='0' and r['status']=='0'}
replayed=[r for r in cases.values() if 20000<=r['index']<30000]
assert len(expected)==len(replayed)==462
assert expected=={(r['family'],r['sample'],r['mask']) for r in replayed}
samples={(r['family'],r['source']+'-'+r['seed']):[float(r['x'+str(i)]) for i in range(64)] for r in csv.DictReader((source/'seeded-samples.csv').open())}
assert all(r['data']==samples[r['family'],r['sample']] for r in replayed)
refs={r['index']:r for r in map(json.loads,(D/'stationary.jsonl').read_text().splitlines())}
errors=json.loads((D/'coefficient-errors.json').read_text());modes={}
assert set(refs)==set(cases)
assert not [r for r in refs.values() if 'open' in r]
for mode in ['native','double']:
 before={r['index']:r for r in map(json.loads,(D/f'before-{mode}.jsonl').read_text().splitlines())}
 after={r['index']:r for r in map(json.loads,(D/f'after-{mode}.jsonl').read_text().splitlines())}
 assert set(before)==set(after)==set(cases)
 assert all(r["logLik"] is not None and math.isfinite(r["logLik"]) for r in after.values() if not r["error"])
 new_stops=[i for i,r in after.items() if r['error'] and not before[i]['error']]
 assert not new_stops,new_stops
 certified=[i for i,r in before.items() if not r['error'] and r.get('certified')==1]
 raw_before={json.loads(line)['index']:line for line in (D/f'before-{mode}.jsonl').read_text().splitlines()}
 raw_after={json.loads(line)['index']:line for line in (D/f'after-{mode}.jsonl').read_text().splitlines()}
 changed=[i for i in certified if raw_before[i]!=raw_after[i]]
 assert not changed,changed
 own=[];inherited=[];preserved=[]
 for r in errors:
  if r['mode']!='after-'+mode:continue
  c=cases[r['index']]
  if c['family']=='sged' and c['held'].get('xi')==1 and 'nu' not in c['held']:
   inherited.append(r)
  elif r.get('certified')==1:preserved.append(r)
  else:own.append(r)
 bad=[r for r in own if r['maximum_scaled_error']>1e-10]
 assert not bad,bad
 g_before=list(map(json.loads,(D/f'golden-before-{mode}.jsonl').read_text().splitlines()))
 g_after=list(map(json.loads,(D/f'golden-after-{mode}.jsonl').read_text().splitlines()))
 assert g_before==g_after,(g_before,g_after)
 assert (D/f'golden-before-{mode}.jsonl').read_text()==(D/f'golden-after-{mode}.jsonl').read_text()
 modes[mode]=dict(cases=len(cases),successful=len([r for r in after.values() if not r['error']]),new_stops=new_stops,new_finite=[i for i,r in after.items() if not r['error'] and before[i]['error']],certified_unchanged=len(certified),golden_defaults_text_identical=len(g_before),owned_uncertified=len(own),maximum_owned_scaled_error=max(r['maximum_scaled_error'] for r in own),preserved_certified_misses=[r for r in preserved if r['maximum_scaled_error']>1e-10],inherited_GED=inherited)
# The original 121 held misses are adjudicated independently, using the
# family's declared location floor rather than division by a near-zero mean.
original=json.loads((D.parent/'2026-11-10-acc-allfree-data/replay-summary.json').read_text())
wanted=set(sum([original[f]['remaining_held_misses'] for f in ['slash','sstd','sged','st']],[]))
assert len(wanted)==121
adjudicated=[]
for i in sorted(wanted):
 row=dict(index=i,family=cases[i]['family'])
 for mode in ['before-native','after-native','after-double']:
  r=next((v for v in errors if v['mode']==mode and v['index']==i),None)
  row[mode]=None if r is None else r['maximum_scaled_error']
 row['before_miss_confirmed']=row['before-native'] is not None and row['before-native']>1e-10
 row['preserved_certified']=next(r for r in map(json.loads,(D/'after-native.jsonl').read_text().splitlines()) if r['index']==i).get('certified')==1
 adjudicated.append(row)
(D/'held-adjudication.json').write_text(json.dumps(adjudicated,indent=2)+'\n')
(D/'verification.json').write_text(json.dumps(dict(precision_digits=90,modes=modes,held_misses=121,held_confirmed=sum(r['before_miss_confirmed'] for r in adjudicated)),indent=2)+'\n')
print(json.dumps(modes,indent=2))
