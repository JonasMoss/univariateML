#!/usr/bin/env python3
"""Run under /tmp/umvl-cpu/run4; keep commands foreground and retain results."""
import json,subprocess,sys,time
from pathlib import Path
out=Path(__file__).resolve().parent
root=out.parents[3]
phase=sys.argv[1]
commands={
 'dev': [('cpp-dev',['just','cpp-test']),('r-dev',['just','test']),('js-build',['just','js-build']),('js-test',['just','js-test'])],
 'full': [('cpp-full',['env','UMVL_TEST_TIER=full','just','cpp-test']),('r-full',['just','test-full']),('cpp-no-long-double',['just','cpp-test-no-long-double']),('js-build',['just','js-build']),('js-test',['just','js-test'])]
}[phase]
ledger_path=out/'checks.json'
ledger=json.loads(ledger_path.read_text()) if ledger_path.exists() else []
for name,cmd in commands:
 start=time.monotonic();log=root/'build/scratch'/f'final-{name}.log'
 print('RUN',name,flush=True)
 with log.open('w') as f:r=subprocess.run(cmd,cwd=root,stdout=f,stderr=subprocess.STDOUT)
 row={'name':name,'command':cmd,'machine':'workstation GCC 15.2','exit_code':r.returncode,'seconds':round(time.monotonic()-start,2)}
 ledger.append(row);ledger_path.write_text(json.dumps(ledger,indent=2)+'\n')
 print('FINISHED',name,'exit',r.returncode,'seconds',row['seconds'],flush=True)
 if r.returncode:sys.exit(r.returncode)
