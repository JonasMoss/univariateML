"""Freeze final snapshot evidence and audit the scoped allowlist delta."""
import collections,gzip,json,pathlib,shutil
D=pathlib.Path(__file__).resolve().parent
modes=['native','double','routes-native','routes-double']
hashes=set()
for mode in modes:
 source=pathlib.Path('build/snapshots')/mode
 provenance=json.loads((source/'provenance.json').read_text())
 hashes.add(provenance['source_sha256'])
 shutil.copyfile(source/'provenance.json',D/(mode+'-provenance.json'))
 with gzip.open(D/(mode+'.jsonl.gz'),'wt') as out:
  for path in sorted(source.glob('*.jsonl')):out.write(path.read_text())
assert len(hashes)==1,hashes
base=json.loads((D/'strict-known-original.json').read_text())['known']
new=json.loads(pathlib.Path('dev/validation/strict-known.json').read_text())['known']
assert not new.keys()-base.keys()
F=set('pois binom nbinom geom zeta lgser norm exp gamma laplace'.split())
def scoped(key):return key.split('|')[2].split('+')[0].strip() in F
def target(key):return scoped(key) and base[key]['ref'] in {'TASK-214.15','TASK-214.16','TASK-214.17','TASK-214.18'}
removed=base.keys()-new.keys()
assert len(removed)==44,len(removed)
assert all(target(k) for k in removed)
assert not any(target(k) for k in new)
assert all(new[k]==v for k,v in base.items() if k not in removed)
summary=dict(starting_commit='da2174a11af7d31c47434a54a65d4108c6b00388',source_sha256=next(iter(hashes)),removed=dict(collections.Counter(base[k]['ref'] for k in removed)),remaining_scoped=dict(collections.Counter(v['ref'] for k,v in new.items() if scoped(k))),removed_entries={k:base[k] for k in sorted(removed)},remaining_scoped_entries={k:v for k,v in new.items() if scoped(k)})
(D/'strict-delta.json').write_text(json.dumps(summary,indent=2)+'\n')
print('Archived four final snapshots; exactly 44 required known entries removed; all others preserved.')
seam_hashes=set();seam_counts={}
for mode in ['native','double']:
 source=pathlib.Path('build/strict/seam')/mode
 provenance=json.loads(source.with_suffix('.provenance.json').read_text())
 seam_hashes.add(provenance['source_sha256'])
 shutil.copyfile(source.with_suffix('.provenance.json'),D/('seam-'+mode+'-provenance.json'))
 rows=[json.loads(s) for s in source.with_suffix('.jsonl').read_text().splitlines()]
 retry_path=source.with_name(mode+'-retry').with_suffix('.jsonl')
 if retry_path.exists():
  retry_provenance=retry_path.with_suffix('.provenance.json')
  assert json.loads(retry_provenance.read_text())['source_sha256']==provenance['source_sha256']
  shutil.copyfile(retry_provenance,D/('seam-'+mode+'-retry-provenance.json'))
  retry={r['index']:r for r in map(json.loads,retry_path.read_text().splitlines()) if r['source']=='archive'}
  rows=[retry.get(r['index'],r) if r['source']=='archive' and r['outcome']=='open fit' else r for r in rows]
 seam_counts[mode]=dict(collections.Counter(r['outcome'] for r in rows))
 with gzip.open(D/('seam-'+mode+'-scope.jsonl.gz'),'wt') as out:
  for r in rows:
   if r['family'] in F|{'weibull','rayleigh','pareto','lomax'}:out.write(json.dumps(r,sort_keys=True)+'\n')
assert len(seam_hashes)==1,seam_hashes
(D/'seam-counts.json').write_text(json.dumps(seam_counts,indent=2)+'\n')
