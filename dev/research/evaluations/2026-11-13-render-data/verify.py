#!/usr/bin/env python3
"""Replay snapshot classifications, ordinary diffs, outcome checks and strict judge.
Run through /tmp/umvl-cpu/run; all children run sequentially on that one slot.
Exit codes are recorded rather than hidden; known scope conflicts remain failures.
"""
import gzip,json,shutil,subprocess,sys
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4]
HERE=Path(__file__).resolve().parent
BASE=ROOT/'dev/research/evaluations/2026-11-13-fit-snapshot-data'
checks=[]
def run(label,args,compressed=False):
 path=HERE/(label+'.log'+('.gz' if compressed else ''))
 command=[sys.executable,*map(str,args)]
 if compressed:
  with gzip.open(path,'wb') as log:
   with subprocess.Popen(command,cwd=ROOT,stdout=subprocess.PIPE,stderr=subprocess.STDOUT) as child:
    shutil.copyfileobj(child.stdout,log);code=child.wait()
 else:
  with path.open('wb') as log:code=subprocess.run(command,cwd=ROOT,stdout=log,stderr=subprocess.STDOUT).returncode
 checks.append(dict(check=label,command=command,exit_code=code,machine='workstation',log=path.name))
 print(label,code,flush=True)
for mode in ('native','double','routes-native','routes-double'):
 run('classify-'+mode,[HERE/'classify.py',BASE/(mode+'.jsonl.gz'),HERE/(mode+'.jsonl.gz'),'--output',HERE/('classification-'+mode+'.json.gz')])
 run('diff-'+mode,[ROOT/'dev/validation/fit-snapshot.py','--diff',BASE/(mode+'.jsonl.gz'),HERE/(mode+'.jsonl.gz')],True)
 run('outcomes-'+mode,[ROOT/'dev/validation/fit-snapshot.py','--outcomes',HERE/(mode+'.jsonl.gz')])
run('fit-judge',[ROOT/'dev/validation/fit-judge.py',str(HERE/'native.jsonl.gz')+','+str(HERE/'routes-native.jsonl.gz'),str(HERE/'double.jsonl.gz')+','+str(HERE/'routes-double.jsonl.gz')])
run('assertion-inventory',[HERE/'assertion-inventory.py'])
run('payload-inventory',[HERE/'summarize.py'])
(HERE/'checks.json').write_text(json.dumps(checks,indent=1)+'\n')
