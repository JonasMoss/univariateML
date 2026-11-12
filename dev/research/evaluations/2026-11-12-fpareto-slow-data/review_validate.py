"""Requested review checks only: both C++ modes, full-tier R files, targeted JS.
Invoke through run4; no nested CPU reservation.
"""
import datetime,json,os,subprocess,time
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4];HERE=Path(__file__).resolve().parent
os.chdir(ROOT);env=os.environ.copy();tmp=str(ROOT/'build/tmp');env.update(TMPDIR=tmp,JUST_TEMPDIR=tmp,XDG_RUNTIME_DIR=tmp,UMVL_TEST_FAMILIES='fpareto,gb2',UMVL_TEST_TIER='full')
for k in ['FIT_FAMILY','JS_FAMILIES']:env.pop(k,None)
pattern='^distribution_(fpareto|gb2)(_slow)?$'
commands=[]
for mode,build in [('native','univariateML-cpp/build'),('double','build/cpp-no-long-double')]:
 commands += [(mode+'-build',['cmake','--build',build,'--target','distribution_tests','-j4']),(mode+'-cpp',['ctest','--test-dir',build,'--output-on-failure','--no-tests=error','-L','full','-R',pattern])]
commands += [('r',['just','test']),('js-build',['just','js-build']),('js',['just','js-test'])]
records=[]
for name,cmd in commands:
 start=time.monotonic();utc=datetime.datetime.now(datetime.timezone.utc).isoformat();log=ROOT/'build/scratch/fpareto-review'/('check-'+name+'.log');print('START',name,utc,flush=True)
 with log.open('w') as f:r=subprocess.run(cmd,env=env,stdout=f,stderr=subprocess.STDOUT)
 records.append(dict(name=name,command=cmd,machine='local workstation',start=utc,seconds=time.monotonic()-start,exit=r.returncode,log=str(log.relative_to(ROOT))))
 (HERE/'review-validation.json').write_text(json.dumps(records,indent=2)+'\n');print('END',name,r.returncode,flush=True)
 if r.returncode:raise SystemExit(r.returncode)
