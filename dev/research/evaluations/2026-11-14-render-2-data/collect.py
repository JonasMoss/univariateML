#!/usr/bin/env python3
"""Collect the final render-2 evidence beside the report and rerun its checks.

Copies the four final snapshots (build/snap3/<mode>) and their provenance,
gzips the suite logs from build/scratch, reruns classify.py, the --outcomes
checks and fit-judge.py, and records every command with its exit code and
machine in checks.json. Baseline: ../2026-11-13-fit-snapshot-data/.
"""
import gzip,json,shutil,subprocess,sys
from pathlib import Path
HERE=Path(__file__).resolve().parent
ROOT=HERE.parents[3]
BASE=HERE.parent/'2026-11-13-fit-snapshot-data'
SNAP=ROOT/'build/snap3'
SCRATCH=ROOT/'build/scratch'
MODES=('native','double','routes-native','routes-double')
def gz(src,dst):
 with open(src,'rb') as a,gzip.open(dst,'wb') as b:shutil.copyfileobj(a,b)
records=[]
def run(name,cmd,log):
 p=subprocess.run(cmd,cwd=ROOT,capture_output=True,text=True)
 (HERE/log).write_text(p.stdout+p.stderr)
 records.append(dict(check=name,command=[str(c) for c in cmd],exit_code=p.returncode,machine='workstation',log=log))
for m in MODES:
 shutil.copyfile(SNAP/m/'snapshot.jsonl.gz',HERE/(m+'.jsonl.gz'))
 shutil.copyfile(SNAP/m/'provenance.json',HERE/(m+'-provenance.json'))
 gz(SCRATCH/('snap3-%s.log'%m),HERE/('snapshot-%s.log.gz'%m))
 run('classify-'+m,[sys.executable,HERE/'classify.py',BASE/(m+'.jsonl.gz'),HERE/(m+'.jsonl.gz'),'--output',HERE/('classification-%s.json.gz'%m)],'classify-%s.log'%m)
 run('outcomes-'+m,[sys.executable,ROOT/'dev/validation/fit-snapshot.py']+(['--modifier-routes'] if m.startswith('routes') else [])+['--outcomes',HERE/(m+'.jsonl.gz')],'outcomes-%s.log'%m)
run('fit-judge',[sys.executable,ROOT/'dev/validation/fit-judge.py',
 '%s,%s'%(HERE/'native.jsonl.gz',HERE/'routes-native.jsonl.gz'),'%s,%s'%(HERE/'double.jsonl.gz',HERE/'routes-double.jsonl.gz')],'fit-judge.log')
run('summarize',[sys.executable,HERE/'summarize.py'],'summary.log')
run('assertion-changes',[sys.executable,HERE/'assertion-changes.py','--json',HERE/'changed-assertions.json','--md',HERE/'changed-assertions.md'],'assertion-changes.log')
fingerprints={json.loads((HERE/(m+'-provenance.json')).read_text())['source_sha256'] for m in MODES}
assert len(fingerprints)==1,'snapshot source fingerprints differ'
(HERE/'checks.json').write_text(json.dumps(records,indent=1)+'\n')
print('source',fingerprints.pop());[print(r['check'],r['exit_code']) for r in records]
