"""Run through /tmp/umvl-cpu/run4 on the workstation (simbox off).
All commands run synchronously; stop at the first failure. Logs are scratch.
"""
import json,os,subprocess,time
from pathlib import Path
root=Path(__file__).resolve().parents[4];os.chdir(root)
scratch=root/'build/scratch/slow-fits-final';scratch.mkdir(parents=True,exist_ok=True)
for k in ('TMPDIR','JUST_TEMPDIR','XDG_RUNTIME_DIR'):os.environ[k]=str(root/'build/tmp')
commands=['cpp-test','test','test-full','cpp-test-no-long-double','js-build','js-test','site-check']
results=[]
for command in commands:
 print('START',command,flush=True);start=time.monotonic()
 with (scratch/(command+'.log')).open('w') as log:
  p=subprocess.run(['just',command],stdout=log,stderr=subprocess.STDOUT)
 row=dict(command='just '+command,machine='workstation',exit=p.returncode,seconds=round(time.monotonic()-start,3),log=str((scratch/(command+'.log')).relative_to(root)))
 results.append(row);Path(__file__).with_name('final-suites.json').write_text(json.dumps(results,indent=2)+'\n');print(row,flush=True)
 if p.returncode:raise SystemExit(p.returncode)
