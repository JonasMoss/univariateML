"""Blocking single-slot recheck of independent numerical evidence."""
import json,subprocess,sys,time
from pathlib import Path
here=Path(__file__).resolve().parent
receipts=[]
for name in ['compare.py','check-reference-preservation.py','regressions.py','check-regressions.py','check-beta-regressions.py','check-likelihoods.py','check-witness.py']:
 started=time.monotonic();result=subprocess.run([sys.executable,str(here/name)],capture_output=True,text=True)
 receipts.append(dict(script=name,returncode=result.returncode,seconds=time.monotonic()-started,stdout=result.stdout,stderr=result.stderr))
 (here/'evidence-checks.json').write_text(json.dumps(receipts,indent=2)+'\n')
 print(name,result.returncode,result.stdout.strip(),flush=True)
 if result.returncode:raise SystemExit(result.returncode)
