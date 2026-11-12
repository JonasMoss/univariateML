"""Focused final checks, run only after the full-suite runner has exited."""
import json,os,subprocess,time
from pathlib import Path
HERE=Path(__file__).resolve().parent;ROOT=HERE.parents[3]
os.chdir(ROOT)
for key in ('TMPDIR','JUST_TEMPDIR','XDG_RUNTIME_DIR'):os.environ[key]=str(ROOT/'build/tmp')
runs=[]
commands=[
 ('invgauss-double-census',['python3','dev/validation/seam-check.py','--family','invgauss','--no-long-double','--output',str(HERE/'after-invgauss-double.jsonl')]),
 ('invgauss-maximized-value',['python3',str(HERE/'invgauss-max-check.py')]),
 ('beta-r-dev',['Rscript',str(HERE/'final-beta-r.R')]),
]
for name,cmd in commands:
    started=time.monotonic()
    print('START',name,flush=True)
    with (ROOT/'build/scratch/seam-check'/('final-'+name+'.log')).open('w') as log:
        result=subprocess.run(['/tmp/umvl-cpu/run',*cmd],stdout=log,stderr=subprocess.STDOUT)
    runs.append(dict(name=name,machine='workstation',command=cmd,returncode=result.returncode,elapsed_seconds=time.monotonic()-started))
    (HERE/'final-post-runs.json').write_text(json.dumps(runs,indent=2)+'\n')
    print('END',name,result.returncode,flush=True)
    if result.returncode:raise SystemExit(result.returncode)
