"""Summarize observed timings; censoring is never a budget-stop classification."""
import csv,json
from pathlib import Path
base=Path(__file__).resolve().parent
raw=[json.loads(s) for s in (base/'stress-final8.jsonl').read_text().splitlines()]
extra=[json.loads(s) for s in (base/'stress-extended.jsonl').read_text().splitlines()]
cmb=[json.loads(s) for s in (base/'stress-cmb-final.jsonl').read_text().splitlines()]
assert len(raw)==129 and len(extra)==6 and len(cmb)==5
assert cmb[0]['source_headers_sha256']==(base/'final-header-sha256.txt').read_text().strip()
records={(r['family'],r['mode']):r for r in raw[1:] if r['family']!='cmb'}
for r in cmb[1:]:records[r['family'],r['mode']]=r
for r in extra[1:]:records[r['family'],r['mode']]=r
assert len(records)==128
solver=[json.loads(s) for s in (base/'stress-solver-final11.log').read_text().splitlines() if s.startswith('{')]
assert len(solver)==145, len(solver)
assert solver[0]['source_headers_sha256']==(base/'final-header-sha256.txt').read_text().strip()
for r in solver[1:]:records[r['family'],r['mode']]=r
retry=[json.loads(s) for s in (base/'stress-solver-retry12.log').read_text().splitlines() if s.startswith('{')]
assert len(retry)==9, len(retry)
assert retry[0]['source_headers_sha256']==solver[0]['source_headers_sha256']
for r in retry[1:]:records[r['family'],r['mode']]=r
assert len(records)==272
rows=[]
for family in sorted({k[0] for k in records}):
 stops=[];returned=[];censored=[]
 for mode in range(4):
  r=records[family,mode]
  if r.get('censored'):censored.append(f'{mode}>{r["limit_seconds"]}s');continue
  assert r['returncode']==0,r
  parts=r['output'].split('\t');seconds=float(parts[3]);kind=parts[4] if len(parts)>4 else ''
  returned.append(seconds)
  if kind=='budget':stops.append((seconds,mode))
 rows.append(dict(family=family,n=100000,seed=143,slowest_observed_budget_seconds=max(stops)[0] if stops else '',budget_mode=max(stops)[1] if stops else '',budget_stops=len(stops),longest_returned_seconds=max(returned) if returned else '',unresolved_censored_modes=','.join(censored)))
with (base/'stress-summary.csv').open('w') as f:
 writer=csv.DictWriter(f,fieldnames=list(rows[0]));writer.writeheader();writer.writerows(rows)
(base/'stress-combined.json').write_text(json.dumps(dict(source_equivalence=json.loads((base/'phase8-equivalence.json').read_text()),records=list(records.values()),summary=rows),indent=2)+'\n')
print(json.dumps(rows,indent=2))
