"""Foreground final suites with requested reservations and simbox fallback.

Run without an enclosing CPU reservation: this driver waits for children.
"""
from pathlib import Path
import subprocess, os, json, time
root=Path.cwd();p=root/'dev/research/evaluations/2026-11-09-emg-perf-data'
scratch=root/'build/scratch/emg-perf';scratch.mkdir(parents=True,exist_ok=True)
env=os.environ.copy()
for key in ['TMPDIR','JUST_TEMPDIR','XDG_RUNTIME_DIR']:env[key]=str(root/'build/tmp')
results=[]
def run(name,cmd,remote=True):
    path=scratch/(name+'.log');start=time.time();machine='simbox' if remote else 'workstation'
    wrapped=['/tmp/umvl-cpu/simbox-run','env','CMAKE_BUILD_PARALLEL_LEVEL=4','CTEST_PARALLEL_LEVEL=4',*cmd] if remote else ['/tmp/umvl-cpu/run4',*cmd]
    print(name,machine,flush=True)
    with path.open('w') as log:r=subprocess.run(wrapped,env=env,stdout=log,stderr=subprocess.STDOUT)
    if remote and r.returncode==75:
        print(name,'simbox unavailable; falling back to workstation',flush=True);machine='workstation'
        with path.open('a') as log:r=subprocess.run(['/tmp/umvl-cpu/run4',*cmd],env=env,stdout=log,stderr=subprocess.STDOUT)
    # Keep small durable summaries, with full logs only for failures.
    lines=path.read_text(errors='replace').splitlines()
    summary='\n'.join(lines[-100:])+'\n'
    (p/(name+'-summary.txt')).write_text(summary)
    results.append(dict(name=name,command=cmd,machine=machine,exit_code=r.returncode,elapsed_seconds=time.time()-start))
    (p/'final-suites.json').write_text(json.dumps(results,indent=2)+'\n')
    if r.returncode:raise SystemExit(f'{name} failed: see {path}')
    print(name,'green',machine,flush=True)
    return machine
run('cpp-dev',['just','cpp-test'],False)
run('cpp-full',['env','UMVL_TEST_TIER=full','just','cpp-test'],False)
mode_machine=run('cpp-no-long-double',['just','cpp-test-no-long-double'])
if mode_machine!='workstation':run('cpp-no-long-double-workstation',['just','cpp-test-no-long-double'],False)
run('r-dev',['just','test'])
run('r-full',['just','test-full'])
run('site',['just','site-check'],False)
run('quality-emg',['just','quality','emg'],False)
print('All final suites completed green.',flush=True)
