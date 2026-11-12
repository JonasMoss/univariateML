"""Run all retained numerical checks under one local single-slot reservation."""
import json,subprocess,time
from pathlib import Path
p=Path(__file__).resolve().parent;root=p.parents[3]
commands=[['compare.py'],['check_witnesses.py'],['check_witnesses.py','--input','final-double.jsonl'],['check_faces.py'],['check_faces.py','--input','final-double.jsonl']]
records=[]
for i,args in enumerate(commands):
 start=time.monotonic();log=root/'build/scratch'/f'evidence-check-{i}.log'
 with log.open('w') as out:r=subprocess.run(['python3',str(p/args[0]),*args[1:]],stdout=out,stderr=subprocess.STDOUT)
 records.append(dict(command=args,exit=r.returncode,seconds=time.monotonic()-start,machine='local workstation'))
 (p/'evidence-validation.json').write_text(json.dumps(records,indent=2)+'\n')
 if r.returncode:raise SystemExit(r.returncode)
print('All five evidence checks passed.')
