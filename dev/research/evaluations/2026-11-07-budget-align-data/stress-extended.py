"""Extend one censored construction per family on the frozen numerical code."""
import json,subprocess,time
from pathlib import Path
base=Path(__file__).resolve().parent
rows=[json.loads(s) for s in (base/'stress-final8.jsonl').read_text().splitlines()]
assert len(rows)==129
print(json.dumps(dict(rows[0],timeout_seconds=180,selection='first censored mode per family with initial limit below 180 seconds')),flush=True)
selected={}
for r in rows[1:]:
 if r.get('censored') and r['limit_seconds']<180:selected.setdefault(r['family'],r['mode'])
for family,mode in selected.items():
 start=time.monotonic()
 try:
  p=subprocess.run(['build/scratch/budget-stress-final',family,str(mode)],text=True,capture_output=True,timeout=180)
  result=dict(family=family,mode=mode,elapsed=time.monotonic()-start,returncode=p.returncode,output=p.stdout.strip(),stderr=p.stderr.strip())
 except subprocess.TimeoutExpired:
  result=dict(family=family,mode=mode,elapsed=time.monotonic()-start,censored=True,limit_seconds=180)
 print(json.dumps(result),flush=True)
