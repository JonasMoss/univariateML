import json,subprocess,time
from pathlib import Path
base=Path(__file__).resolve().parent
rows=[json.loads(s) for s in (base/'stress-frozen.jsonl').read_text().splitlines()]
for r in rows:
 if not r.get('censored'):continue
 start=time.monotonic()
 try:
  p=subprocess.run(['build/scratch/budget-stress',r['family'],str(r['mode'])],text=True,capture_output=True,timeout=180)
  result=dict(family=r['family'],mode=r['mode'],elapsed=time.monotonic()-start,returncode=p.returncode,output=p.stdout.strip(),stderr=p.stderr.strip())
 except subprocess.TimeoutExpired:
  result=dict(family=r['family'],mode=r['mode'],elapsed=time.monotonic()-start,censored=True,limit_seconds=180)
 print(json.dumps(result),flush=True)
