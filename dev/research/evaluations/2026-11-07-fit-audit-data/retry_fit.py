#!/usr/bin/env python3
"""Retry wall-clock guarded core calls without changing fitter work controls."""
import json,pathlib,subprocess,time,os
HERE=pathlib.Path(__file__).resolve().parent;ROOT=HERE.parents[3]
cases=json.loads((HERE/'cases.json').read_text());rows={int(l.split(',')[2]):l for l in (HERE/'fits.csv').read_text().splitlines()}
if not (HERE/'fits-initial.csv').exists():(HERE/'fits-initial.csv').write_text((HERE/'fits.csv').read_text())
budget=float(os.environ.get('CORE_RETRY_SECONDS','20'))
previous={json.loads(l)['index'] for l in (HERE/'fit-retries.jsonl').read_text().splitlines()} if (HERE/'fit-retries.jsonl').exists() else set()
with (HERE/'fit-retries.jsonl').open('a') as log:
 for i,row in list(rows.items()):
  if row.split(',')[3]!='-997' or i in previous and os.environ.get('AUDIT_RETRY_AGAIN')!='1':continue
  c=cases[i];line=f"{c['family']} {c['mask']} {i} {len(c['data'])} "+' '.join(map(str,c['data']))+'\n';start=time.monotonic()
  try:
   run=subprocess.run([str(ROOT/'build/scratch/fit-audit/stress')],input=line,text=True,capture_output=True,timeout=budget)
   rows[i]=run.stdout.strip() or row
  except subprocess.TimeoutExpired:pass
  record=dict(index=i,family=c['family'],seconds=time.monotonic()-start,wall_budget=budget,status=int(rows[i].split(',')[3]));log.write(json.dumps(record)+'\n');log.flush();print(record,flush=True)
  tmp=HERE/'fits-retry.tmp';tmp.write_text('\n'.join(rows[j] for j in sorted(rows))+'\n');tmp.replace(HERE/'fits.csv')
