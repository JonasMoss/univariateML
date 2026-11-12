"""Run required suites in the foreground; preserve logs, timing and machine."""
import datetime,json,os,subprocess,time
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4];HERE=Path(__file__).resolve().parent
os.chdir(ROOT)
for key in ['TMPDIR','JUST_TEMPDIR','XDG_RUNTIME_DIR']:os.environ[key]=str(ROOT/'build/tmp')
(ROOT/'build/tmp').mkdir(parents=True,exist_ok=True)
logs=ROOT/'build/scratch/seam-check/final-logs';logs.mkdir(parents=True,exist_ok=True)
runs=[]
for name,remote,recipe in [
    ('cpp-test',True,'cpp-test'),
    ('simbox-cpp-test-no-long-double',True,'cpp-test-no-long-double'),
    ('workstation-cpp-test-no-long-double',False,'cpp-test-no-long-double'),
    ('test',True,'test'),
    ('test-full',True,'test-full'),
]:
    started=time.monotonic();stamp=datetime.datetime.now(datetime.timezone.utc).isoformat()
    machine='simbox' if remote else 'workstation'
    cmd=['/tmp/umvl-cpu/simbox-run','env','CMAKE_BUILD_PARALLEL_LEVEL=4','CTEST_PARALLEL_LEVEL=4','just',recipe] if remote else ['/tmp/umvl-cpu/run4','just',recipe]
    print('START',name,machine,flush=True)
    with (logs/(name+'.log')).open('w') as out:
        result=subprocess.run(cmd,stdout=out,stderr=subprocess.STDOUT)
        if remote and result.returncode==75:
            machine='workstation (simbox exit 75)';out.write('\nSIMBOX EXIT 75: LOCAL FALLBACK\n');out.flush()
            result=subprocess.run(['/tmp/umvl-cpu/run4','just',recipe],stdout=out,stderr=subprocess.STDOUT)
    runs.append(dict(name=name,recipe=recipe,machine=machine,returncode=result.returncode,
        started=stamp,elapsed_seconds=time.monotonic()-started))
    (HERE/'final-runs.json').write_text(json.dumps(runs,indent=2)+'\n')
    print('END',name,machine,'exit',result.returncode,'seconds',round(runs[-1]['elapsed_seconds'],2),flush=True)
raise SystemExit(int(any(r['returncode'] for r in runs)))
