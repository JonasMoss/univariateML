"""Final local suites. Invoke under /tmp/umvl-cpu/run4 (no nested locks)."""
import datetime,json,os,subprocess,time
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4];HERE=Path(__file__).resolve().parent
os.chdir(ROOT)
env=os.environ.copy();tmp=str(ROOT/'build/tmp');env.update(TMPDIR=tmp,JUST_TEMPDIR=tmp,XDG_RUNTIME_DIR=tmp)
for key in ['UMVL_TEST_FAMILIES','JS_FAMILIES','FIT_FAMILY','UMVL_TEST_TIER']:env.pop(key,None)
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True)
configs=[['cmake','-S','univariateML-cpp','-B',build,'-DCMAKE_BUILD_TYPE=','-DCMAKE_CXX_FLAGS_RELEASE=-O3 -DNDEBUG','-DCMAKE_CXX_COMPILER_LAUNCHER=ccache','-DBoost_INCLUDE_DIR='+boost]+(['-DCMAKE_CXX_FLAGS=-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'] if 'no-long-double' in build else []) for build in ['univariateML-cpp/build','build/cpp-no-long-double']]
commands=configs+[['just','cpp-test'],['env','UMVL_TEST_TIER=full','just','cpp-test'],['just','cpp-test-no-long-double'],['just','site-check']]
results=[]
for cmd in commands:
 name=('configure-native' if cmd[4]=='univariateML-cpp/build' else 'configure-double') if cmd[0]=='cmake' else ('cpp-test-full' if cmd[0]=='env' else cmd[1]);log=ROOT/'build/scratch'/('final-'+name+'.log');start=time.monotonic();utc=datetime.datetime.now(datetime.timezone.utc).isoformat()
 print('START',name,utc,flush=True)
 with log.open('w') as f:p=subprocess.run(cmd,env=env,stdout=f,stderr=subprocess.STDOUT)
 results.append(dict(command=cmd,machine='local workstation (simbox off by brief)',start=utc,seconds=time.monotonic()-start,exit=p.returncode,log=str(log.relative_to(ROOT))))
 (HERE/'validation.json').write_text(json.dumps(results,indent=2)+'\n')
 print('END',name,p.returncode,'seconds',results[-1]['seconds'],flush=True)
 if p.returncode:raise SystemExit(p.returncode)
