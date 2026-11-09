#!/usr/bin/env python3
"""Run one saved core case and print its high precision evidence."""
import json,subprocess,sys,pathlib
HERE=pathlib.Path(__file__).resolve().parent
ROOT=HERE.parents[3]
c=json.loads((HERE/'cases.json').read_text())[int(sys.argv[1])]
print(json.dumps(c,indent=2))
line=f"{c['family']} {c['mask']} {c['index']} {len(c['data'])} "+' '.join(map(str,c['data']))+'\n'
print('stdin:',line,end='')
subprocess.run([str(ROOT/'build/scratch/fit-audit/stress')],input=line,text=True,check=True)
for r in map(json.loads,(HERE/'reference.jsonl').read_text().splitlines()):
 if r['index']==c['index']:print(json.dumps(r,indent=2));break
