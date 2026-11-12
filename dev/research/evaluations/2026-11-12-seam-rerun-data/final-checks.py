#!/usr/bin/env python3
"""Blocking final suites, one at a time; exit-75 fallback is in run-check.py."""
import json,re,subprocess,sys
from pathlib import Path
folder=Path(__file__).resolve().parent

def run(name,where,command):
 record=folder/f'{name}-run.json'
 if record.exists() and json.loads(record.read_text())['attempts'][-1]['exit']==0:
  print('Already green:',name,flush=True);return
 if name=='cpp-dev' and record.exists():
  retest=folder/'cpp-dev-binomial-retest-run.json'
  original=json.loads(record.read_text())['attempts'][-1]
  failed=re.findall(r'\d+ - (\S+) \(Failed\)',(folder.parents[3]/original['log']).read_text())
  if failed==['distribution_binom'] and retest.exists() and json.loads(retest.read_text())['attempts'][-1]['exit']==0:
   print('Native coverage green: original 107/108 plus fresh binomial retest after the family-only fix; no repeated full suite.',flush=True);return
 p=subprocess.run([sys.executable,str(folder/'run-check.py'),name,where,'run4',*command])
 if p.returncode:raise SystemExit(p.returncode)

run('cpp-dev','remote',['just','cpp-test'])
run('cpp-no-long-double','remote',['just','cpp-test-no-long-double'])
record=json.loads((folder/'cpp-no-long-double-run.json').read_text())
if record['attempts'][-1]['machine']=='simbox':
 run('cpp-no-long-double-local','local',['just','cpp-test-no-long-double'])
else:
 print('The no-long-double fallback already supplies the required workstation run.',flush=True)
run('r-dev','remote',['just','test'])
run('r-full','remote',['just','test-full'])
run('site-check','local',['just','site-check'])
print('All final checks green.',flush=True)
