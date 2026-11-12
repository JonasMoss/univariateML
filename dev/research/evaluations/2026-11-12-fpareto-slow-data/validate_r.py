"""Required R tiers, local run4 reservation; do not nest slot locks."""
import datetime,json,os,subprocess,time
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4];HERE=Path(__file__).resolve().parent
os.chdir(ROOT);env=os.environ.copy();tmp=str(ROOT/'build/tmp');env.update(TMPDIR=tmp,JUST_TEMPDIR=tmp,XDG_RUNTIME_DIR=tmp)
for key in ['UMVL_TEST_FAMILIES','JS_FAMILIES','FIT_FAMILY','UMVL_TEST_TIER']:env.pop(key,None)
results=[]
for name in ['test','test-full']:
 cmd=['just',name];log=ROOT/'build/scratch'/('final-'+name+'.log');start=time.monotonic();utc=datetime.datetime.now(datetime.timezone.utc).isoformat()
 print('START',name,utc,flush=True)
 with log.open('w') as f:p=subprocess.run(cmd,env=env,stdout=f,stderr=subprocess.STDOUT)
 results.append(dict(command=cmd,machine='local workstation (simbox off by brief)',start=utc,seconds=time.monotonic()-start,exit=p.returncode,log=str(log.relative_to(ROOT))))
 (HERE/'validation-r.json').write_text(json.dumps(results,indent=2)+'\n')
 print('END',name,p.returncode,'seconds',results[-1]['seconds'],flush=True)
 if p.returncode:raise SystemExit(p.returncode)
