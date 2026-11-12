"""Run only family-local checks after the final ST angular routing fixes."""
import json, os, subprocess, sys, time, shutil
from pathlib import Path
H=Path(__file__).resolve().parent;ROOT=H.parents[3];os.chdir(ROOT)
os.environ.update(TMPDIR=str(ROOT/'build/tmp'),JUST_TEMPDIR=str(ROOT/'build/tmp'),XDG_RUNTIME_DIR=str(ROOT/'build/tmp'))
for key in ['UMVL_TEST_FAMILIES','UMVL_TEST_TIER','JS_FAMILIES','FIT_FAMILY','UMVL_JS_GROUP','UMVL_JS_GROUP_COUNT']:
 os.environ.pop(key,None)
assert json.loads((H/'post-guard-runs.json').read_text())[-1]['exit_code']==0
shutil.copyfile(ROOT/'build/scratch/truth-results.json',H/'js-full-truth-results.json')
records=[]
for label,wrapper,script in [('late-st-build-tests','/tmp/umvl-cpu/run4','late-st-batch.py'),('late-st-site-evidence','/tmp/umvl-cpu/run','late-st-finish.py')]:
 log=ROOT/'build/scratch/dpq-skew/final-logs'/(label+'.log')
 command=[wrapper,'python3',str((H/script).relative_to(ROOT))]
 print('START',label,flush=True);start=time.time()
 with log.open('w') as out:code=subprocess.call(command,stdout=out,stderr=subprocess.STDOUT)
 records.append(dict(label=label,machine='workstation',command=command,exit_code=code,seconds=time.time()-start,log=str(log.relative_to(ROOT))))
 (H/'late-st-runs.json').write_text(json.dumps(records,indent=2)+'\n')
 print('END',label,'exit',code,flush=True)
 print('\n'.join(log.read_text(errors='replace').splitlines()[-16:]),flush=True)
 if code:sys.exit(code)
