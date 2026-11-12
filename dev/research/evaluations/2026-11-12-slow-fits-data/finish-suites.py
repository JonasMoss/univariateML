"""Target the late dgamma-only fix, then finish the required suites.
Run synchronously through run4. The initial full run remains recorded.
"""
import json,os,subprocess,time
from pathlib import Path
root=Path(__file__).resolve().parents[4];os.chdir(root)
p=Path(__file__).resolve().parent;scratch=root/'build/scratch/slow-fits-final'
for k in ('TMPDIR','JUST_TEMPDIR','XDG_RUNTIME_DIR'):os.environ[k]=str(root/'build/tmp')
os.environ.update(CCACHE_DIR=str(Path.home()/'.cache/ccache/univariateML'),CCACHE_BASEDIR=str(root),CCACHE_NOHASHDIR='1',CCACHE_MAXSIZE='8G')
results=json.loads((p/'final-suites.json').read_text())
commands=[
 ('cpp-dgamma-build',['cmake','--build','univariateML-cpp/build','--target','distribution_tests','-j4'],{}),
 ('cpp-dgamma-full',['ctest','--test-dir','univariateML-cpp/build','--output-on-failure','--no-tests=error','-L','full','-R','^distribution_dgamma(_slow)?$'],{}),
 ('test-dgamma-full',['just','test'],dict(UMVL_TEST_FAMILIES='dgamma',UMVL_TEST_TIER='full')),
 *[(name,['just',name],{}) for name in ['cpp-test-no-long-double','js-build','js-test','site-check']]]
for name,command,override in commands:
 print('START',name,flush=True);start=time.monotonic();env=os.environ.copy();env.update(override)
 with (scratch/(name+'.log')).open('w') as log:code=subprocess.call(command,env=env,stdout=log,stderr=subprocess.STDOUT)
 row=dict(command=' '.join(command),name=name,environment=override,machine='workstation',exit=code,seconds=round(time.monotonic()-start,3),log=str((scratch/(name+'.log')).relative_to(root)))
 results.append(row);(p/'final-suites.json').write_text(json.dumps(results,indent=2)+'\n');print(row,flush=True)
 if code:raise SystemExit(code)
