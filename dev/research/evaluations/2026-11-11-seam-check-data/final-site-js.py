"""Check the final dossier/WASM build and JS tests on one pinned CPU."""
import datetime,json,os,subprocess,time
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4];HERE=Path(__file__).resolve().parent
os.chdir(ROOT);os.sched_setaffinity(0,{min(os.sched_getaffinity(0))})
for key in ['TMPDIR','JUST_TEMPDIR','XDG_RUNTIME_DIR']:os.environ[key]=str(ROOT/'build/tmp')
os.environ['GOMAXPROCS']='1';os.environ['UV_THREADPOOL_SIZE']='1'
runs=[]
for recipe in ['site-check','js-test']:
    start=time.monotonic();stamp=datetime.datetime.now(datetime.timezone.utc).isoformat()
    print('START',recipe,flush=True)
    log=ROOT/'build/scratch/seam-check'/('final-'+recipe+'.log')
    with log.open('w') as out:result=subprocess.run(['just',recipe],stdout=out,stderr=subprocess.STDOUT)
    runs.append(dict(recipe=recipe,machine='workstation (one CPU affinity)',returncode=result.returncode,started=stamp,elapsed_seconds=time.monotonic()-start))
    (HERE/'final-extra-runs.json').write_text(json.dumps(runs,indent=2)+'\n')
    print('END',recipe,result.returncode,flush=True)
raise SystemExit(int(any(r['returncode'] for r in runs)))
