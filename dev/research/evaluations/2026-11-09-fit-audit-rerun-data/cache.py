#!/usr/bin/env python3
"""Reuse only exact input and returned-point matches from the frozen audit."""
import os,gzip,json,math,pathlib
HERE=pathlib.Path(__file__).resolve().parent
OLD=HERE.parent/'2026-11-07-fit-audit-data'
assert (HERE/'cases.json').read_bytes()==gzip.decompress((OLD/'cases.json.gz').read_bytes())
old={int(l.split(',')[2]):l.split(',') for l in gzip.decompress((OLD/'fits.csv.gz').read_bytes()).decode().splitlines()}
new={int(l.split(',')[2]):l.split(',') for l in (HERE/'fits.csv').read_text().splitlines()}
def equal(a,b):
 try:
  x,y=float(a),float(b)
  return x==y or math.isnan(x) and math.isnan(y)
 except ValueError:return a==b
reused=[]
for r in map(json.loads,gzip.decompress((OLD/'reference.jsonl.gz').read_bytes()).decode().splitlines()):
 a,b=old[r['index']],new[r['index']]
 if a[3]==b[3] and len(a)==len(b) and all(equal(x.split('=')[-1],y.split('=')[-1]) for x,y in zip(a[8:],b[8:])):
  r['reference_origin']='frozen 2026-11-07 exact returned-point match'
  reused.append(r)
frozen_reused=len(reused)
reused_by_id={r['index']:r for r in reused}
for cachefile in ([] if os.environ.get('AUDIT_BASELINE_CACHE_ONLY')=='1' else [HERE/'reference.jsonl',HERE/'reference-single.jsonl']):
 if not cachefile.exists():continue
 if cachefile.name=='reference-single.jsonl' and not (HERE/'reference-single-ready').exists():continue
 for r in map(json.loads,cachefile.read_text().splitlines()):
  row=new[r['index']]
  if r.get('observed_fit_signature')==[row[3],*row[8:]]:reused_by_id[r['index']]=r
reused=list(reused_by_id.values())
(HERE/'reference.jsonl').write_text(''.join(json.dumps(r)+'\n' for r in reused))
(HERE/'cache-validation.json').write_text(json.dumps(dict(cases=len(new),frozen_reference_reused=frozen_reused,new_returned_points=len(new)-frozen_reused,reused=len(reused),requiring_new_reference=len(new)-len(reused),frozen_input_bytes_identical=True),indent=2))
print('Reused',len(reused),'references;',len(new)-len(reused),'new references required',flush=True)
