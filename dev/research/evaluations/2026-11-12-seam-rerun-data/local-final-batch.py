#!/usr/bin/env python3
"""Active computation only, UNDER ONE run4 lease; never acquire nested slots.

Remote attempts happen in run-check.py before this lease is requested. Each
local suite keeps its own actual command, result, timer, and complete log.
"""
import json,os,subprocess,sys,time
from pathlib import Path
folder=Path(__file__).resolve().parent;root=folder.parents[3];scratch=root/'build/scratch/seam-rerun'
for name,command in [('r-dev',['just','test']),('r-full',['just','test-full']),('site-check',['just','site-check'])]:
 path=folder/f'{name}-run.json'
 record=json.loads(path.read_text()) if path.exists() else dict(name=name,command=command,attempts=[])
 if record['attempts'] and record['attempts'][-1]['exit']==0:
  print('Already green in batch:',name,flush=True);continue
 log=scratch/f'{name}-workstation.log';start=time.time()
 with log.open('w') as out:p=subprocess.run(command,cwd=root,env=os.environ,stdout=out,stderr=subprocess.STDOUT)
 attempt=dict(machine='workstation',command=command,exit=p.returncode,seconds=time.time()-start,log=str(log.relative_to(root)),reservation_record='local-final-batch-reservation.json')
 record['attempts'].append(attempt);path.write_text(json.dumps(record,indent=2)+'\n');print(json.dumps(attempt),flush=True)
 if p.returncode:raise SystemExit(p.returncode)
print('R development, R full, and site checks green.',flush=True)
