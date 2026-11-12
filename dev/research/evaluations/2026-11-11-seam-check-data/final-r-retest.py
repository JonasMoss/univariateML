"""Regenerate and retest the two families touched after the full R suite."""
import json,os,subprocess,time
from pathlib import Path
HERE=Path(__file__).resolve().parent;ROOT=HERE.parents[3];os.chdir(ROOT)
for key in ('TMPDIR','JUST_TEMPDIR','XDG_RUNTIME_DIR'):os.environ[key]=str(ROOT/'build/tmp')
runs=[]
for name,cmd in [('regen',['just','regen']),('r-beta-invgauss-dev',['Rscript',str(HERE/'final-beta-r.R')])]:
    start=time.monotonic()
    with (ROOT/'build/scratch/seam-check'/('current-'+name+'.log')).open('w') as log:
        result=subprocess.run(['/tmp/umvl-cpu/run',*cmd],stdout=log,stderr=subprocess.STDOUT)
    runs.append(dict(name=name,machine='workstation',returncode=result.returncode,elapsed_seconds=time.monotonic()-start))
    (HERE/'final-r-retest-runs.json').write_text(json.dumps(runs,indent=2)+'\n')
    print(name,result.returncode,flush=True)
    if result.returncode:raise SystemExit(result.returncode)
