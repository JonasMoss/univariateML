"""Targeted follow-up after the double-only maximum correction; no full reruns."""
import json,os,subprocess,sys,time
from pathlib import Path
HERE=Path(__file__).resolve().parent;ROOT=HERE.parents[3];os.chdir(ROOT)
os.sched_setaffinity(0,{min(os.sched_getaffinity(0))})
for key in ('TMPDIR','JUST_TEMPDIR','XDG_RUNTIME_DIR'):os.environ[key]=str(ROOT/'build/tmp')
os.environ['GOMAXPROCS']='1';os.environ['UV_THREADPOOL_SIZE']='1'
relative=str(HERE.relative_to(ROOT));script=relative+'/targeted-cpp.sh'
checks=[
 ('cpp-invgauss',['bash',script,'invgauss']),
 ('cpp-invgauss-double',['env','UMVL_SEAM_NO_LONG_DOUBLE=1','bash',script,'invgauss']),
 ('invgauss-census',['python3','dev/validation/seam-check.py','--family','invgauss','--output',relative+'/after-invgauss-final.jsonl']),
 ('invgauss-double-census',['python3','dev/validation/seam-check.py','--family','invgauss','--no-long-double','--output',relative+'/after-invgauss-double.jsonl']),
 ('summary',['python3',relative+'/summarize.py']),
 ('references',['python3',relative+'/reconcile-references.py']),
 ('invgauss-max',['python3',relative+'/invgauss-max-check.py']),
 ('regen',['just','regen']),
 ('r-beta-invgauss-dev',['Rscript',relative+'/final-beta-r.R']),
 ('site-check',['just','site-check']),
 ('js-build',['just','js-build']),
 ('js-invgauss',['node','--v8-pool-size=1','--test','--test-name-pattern=^invgauss |^public facade$','bindings/js/test.mjs']),
 ('wasm-invgauss-max',['node','--v8-pool-size=1',relative+'/invgauss-wasm.mjs']),
]
prefix='verified' if '--final' in sys.argv else 'late'
status=HERE/('final-invgauss-runs.json' if '--final' in sys.argv else 'late-invgauss-runs.json')
if '--final' in sys.argv:checks=[(name,cmd) for name,cmd in checks if name not in ('regen','r-beta-invgauss-dev','site-check')]
runs=json.loads(status.read_text()) if '--resume' in sys.argv else []
runs=[r for r in runs if r['returncode']==0]
for name,cmd in checks:
    if any(r['name']==name for r in runs):continue
    start=time.monotonic();print('START',name,flush=True)
    with (ROOT/'build/scratch/seam-check'/(prefix+'-'+name+'.log')).open('w') as log:
        result=subprocess.run(['/tmp/umvl-cpu/run',*cmd],stdout=log,stderr=subprocess.STDOUT)
    runs.append(dict(name=name,machine='workstation (one CPU affinity)',command=cmd,returncode=result.returncode,elapsed_seconds=time.monotonic()-start))
    status.write_text(json.dumps(runs,indent=2)+'\n')
    print('END',name,result.returncode,flush=True)
    if result.returncode:raise SystemExit(result.returncode)
