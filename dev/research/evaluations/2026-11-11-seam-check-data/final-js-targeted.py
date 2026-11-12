"""Rebuild final WASM and recheck the six beta-dependent fit families."""
import json,os,subprocess,time
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4];HERE=Path(__file__).resolve().parent
os.chdir(ROOT);os.sched_setaffinity(0,{min(os.sched_getaffinity(0))})
for key in ['TMPDIR','JUST_TEMPDIR','XDG_RUNTIME_DIR']:os.environ[key]=str(ROOT/'build/tmp')
os.environ['GOMAXPROCS']='1';os.environ['UV_THREADPOOL_SIZE']='1'
pattern='^(beta|betapr|fpareto|gb2|genbeta|nsbeta|GB2|Feller) |^public facade$|^unit beta-prime fallback|^delegate follow-up'
runs=[]
for name,cmd in [('js-build',['just','js-build']),('js-beta-dependent-tests',['node','--v8-pool-size=1','--test','--test-name-pattern='+pattern,'bindings/js/test.mjs'])]:
    start=time.monotonic();print('START',name,flush=True)
    with (ROOT/'build/scratch/seam-check'/('current-'+name+'.log')).open('w') as out:
        result=subprocess.run(cmd,stdout=out,stderr=subprocess.STDOUT)
    runs.append(dict(name=name,machine='workstation (one CPU affinity)',returncode=result.returncode,elapsed_seconds=time.monotonic()-start))
    (HERE/'final-js-targeted-runs.json').write_text(json.dumps(runs,indent=2)+'\n')
    print('END',name,result.returncode,flush=True)
    if result.returncode:break
raise SystemExit(int(any(r['returncode'] for r in runs)))
