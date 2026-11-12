"""Foreground-only suite driver. Every computing command reserves its own slots."""
import os,subprocess,json,time,gzip,sys
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4];H=Path(__file__).resolve().parent;W=ROOT/'build/scratch/dpq-skew/final-logs';W.mkdir(parents=True,exist_ok=True)
os.chdir(ROOT);os.environ.update(TMPDIR=str(ROOT/'build/tmp'),JUST_TEMPDIR=str(ROOT/'build/tmp'),XDG_RUNTIME_DIR=str(ROOT/'build/tmp'))
for key in ['UMVL_TEST_FAMILIES','FIT_FAMILY','JS_FAMILIES','UMVL_JS_GROUP','UMVL_JS_GROUP_COUNT','UMVL_TEST_TIER']:
 os.environ.pop(key,None)
receipts=[]
def run(label,cmd,machine):
 log=W/(label+'.log');start=time.time();print('START',label,machine,flush=True)
 with log.open('w') as out:code=subprocess.call(cmd,stdout=out,stderr=subprocess.STDOUT)
 elapsed=time.time()-start
 record=dict(label=label,machine=machine,command=cmd,exit_code=code,seconds=elapsed,log=str(log.relative_to(ROOT)))
 receipts.append(record);(H/'final-runs.json').write_text(json.dumps(receipts,indent=2)+'\n')
 print('END',label,machine,'exit',code,'seconds',round(elapsed,1),flush=True)
 print('\n'.join(log.read_text(errors='replace').splitlines()[-14:]),flush=True)
 return code
# Avoid six-worker simbox defaults; this job uses at most four cores.
for recipe in ['cpp-test','cpp-test-no-long-double','test','test-full']:
 cmd=['/tmp/umvl-cpu/simbox-run','env','CMAKE_BUILD_PARALLEL_LEVEL=4','CTEST_PARALLEL_LEVEL=4','just',recipe]
 code=run('simbox-'+recipe,cmd,'simbox')
 if code==75:code=run('workstation-'+recipe,['/tmp/umvl-cpu/run4','just',recipe],'workstation')
 if code:sys.exit(code)
for recipe in ['cpp-test-no-long-double','js-build','js-test','site-check']:
 # The local double-only suite is mandatory even when simbox succeeded.
 if recipe=='cpp-test-no-long-double' and any(r['label']=='workstation-'+recipe and r['exit_code']==0 for r in receipts):continue
 wrapper='/tmp/umvl-cpu/run' if recipe=='site-check' else '/tmp/umvl-cpu/run4'
 code=run('workstation-'+recipe,[wrapper,'just',recipe],'workstation')
 if code:sys.exit(code)
