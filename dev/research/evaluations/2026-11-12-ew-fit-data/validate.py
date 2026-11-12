import os,subprocess,json,time
from pathlib import Path
root=Path.cwd();scratch=root/'build/scratch';results=[];effective=[]
os.environ.update(TMPDIR=str(root/'build/tmp'),JUST_TEMPDIR=str(root/'build/tmp'),XDG_RUNTIME_DIR=str(root/'build/tmp'))
def run(name,remote=False):
 log=scratch/('final-'+name+('-simbox' if remote else '-local')+'.log')
 command=(['/tmp/umvl-cpu/simbox-run','env','CMAKE_BUILD_PARALLEL_LEVEL=4','CTEST_PARALLEL_LEVEL=4'] if remote else ['/tmp/umvl-cpu/run4'])+['just',name]
 start=time.monotonic();print('START',name,'simbox' if remote else 'workstation',flush=True)
 with log.open('w') as f:rc=subprocess.call(command,stdout=f,stderr=subprocess.STDOUT)
 item=dict(command=name,machine='simbox' if remote else 'workstation',exit=rc,seconds=time.monotonic()-start,log=str(log.relative_to(root)))
 results.append(item);(scratch/'final-validation.json').write_text(json.dumps(results,indent=2)+'\n');print('END',item,flush=True)
 if remote and rc==75:return run(name,False)
 if rc:print("FAILED; continuing independent checks",name,rc,flush=True)
 return rc
for name in ['cpp-test','test','test-full','cpp-test-no-long-double']:effective.append(run(name,True))
if not any(r['command']=='cpp-test-no-long-double' and r['machine']=='workstation' and r['exit']==0 for r in results):effective.append(run('cpp-test-no-long-double'))
for name in ['js-build','js-test','site-check']:effective.append(run(name))
if any(effective):raise SystemExit(1)
