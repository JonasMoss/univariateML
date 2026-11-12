"""Focused checks after the owned all-free diagnostic-scope correction.
Run under run4, after the complete native C++ run has finished.
"""
import datetime,json,os,subprocess,time
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4];HERE=Path(__file__).resolve().parent
os.chdir(ROOT);env=os.environ.copy();tmp=str(ROOT/'build/tmp');env.update(TMPDIR=tmp,JUST_TEMPDIR=tmp,XDG_RUNTIME_DIR=tmp,UMVL_TEST_FAMILIES='fpareto,gb2',UMVL_TEST_TIER='full')
for key in ['FIT_FAMILY','JS_FAMILIES']:env.pop(key,None)
commands=[('cpp-build',['cmake','--build','univariateML-cpp/build','--target','distribution_tests','-j4']),('cpp',['ctest','--test-dir','univariateML-cpp/build','--output-on-failure','--no-tests=error','-L','full','-R','^distribution_(fpareto|gb2)(_slow)?$']),('r',['just','test']),('js-build',['just','js-build']),('js',['just','js-test'])]
records=[]
for name,cmd in commands:
 start=time.monotonic();utc=datetime.datetime.now(datetime.timezone.utc).isoformat();log=ROOT/'build/scratch'/('late-'+name+'.log');print('START',name,utc,flush=True)
 with log.open('w') as out:r=subprocess.run(cmd,env=env,stdout=out,stderr=subprocess.STDOUT)
 records.append(dict(name=name,command=cmd,machine='local workstation',start=utc,seconds=time.monotonic()-start,exit=r.returncode,log=str(log.relative_to(ROOT))))
 (HERE/'validation-late.json').write_text(json.dumps(records,indent=2)+'\n');print('END',name,r.returncode,flush=True)
 if r.returncode:raise SystemExit(r.returncode)
