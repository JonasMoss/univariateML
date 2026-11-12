#!/usr/bin/env python3
"""Run a final check synchronously; retain machine, command, exit and log.

Usage: run-check.py NAME local|remote run|run4 COMMAND ...
The remote wrapper's exit 75 is retried on the workstation under run4.
Invoke outside a CPU reservation: this program acquires it only for computing.
"""
import json, os, subprocess, sys, time
from pathlib import Path
root=Path(__file__).resolve().parents[4]
name,where,slots,*command=sys.argv[1:]
folder=Path(__file__).resolve().parent
scratch=root/'build/scratch/seam-rerun';scratch.mkdir(parents=True,exist_ok=True)
env=dict(os.environ,TMPDIR=str(root/'build/tmp'),JUST_TEMPDIR=str(root/'build/tmp'),XDG_RUNTIME_DIR=str(root/'build/tmp'))
record=dict(name=name,command=command,attempts=[])
for machine in (['simbox','workstation'] if where=='remote' else ['workstation']):
    actual=(['/tmp/umvl-cpu/simbox-run','env','CMAKE_BUILD_PARALLEL_LEVEL=4','CTEST_PARALLEL_LEVEL=4'] if machine=='simbox' else ['/tmp/umvl-cpu/'+slots])+command
    log=scratch/f'{name}-{machine}.log'
    start=time.time()
    with log.open('w') as out:
        p=subprocess.run(actual,cwd=root,env=env,stdout=out,stderr=subprocess.STDOUT)
    attempt=dict(machine=machine,command=actual,exit=p.returncode,seconds=time.time()-start,log=str(log.relative_to(root)))
    record['attempts'].append(attempt)
    print(json.dumps(attempt),flush=True)
    if name=='r-dev' and machine=='simbox' and p.returncode==75:
        # Check the other remote R tier BEFORE requesting a local lease. Once
        # acquired, the batch performs only active, sequential computations.
        (folder/f'{name}-run.json').write_text(json.dumps(record,indent=2)+'\n')
        full_path=folder/'r-full-run.json'
        full=json.loads(full_path.read_text()) if full_path.exists() else dict(name='r-full',command=['just','test-full'],attempts=[])
        if not full['attempts'] or full['attempts'][-1]['exit']!=0:
            assert not full['attempts'] or full['attempts'][-1]['exit']==75,'Handle a failed full R test explicitly before retrying.'
            actual_full=['/tmp/umvl-cpu/simbox-run','env','CMAKE_BUILD_PARALLEL_LEVEL=4','CTEST_PARALLEL_LEVEL=4','just','test-full']
            full_log=scratch/'r-full-simbox.log';full_start=time.time()
            with full_log.open('w') as out:remote_full=subprocess.run(actual_full,cwd=root,env=env,stdout=out,stderr=subprocess.STDOUT)
            full_attempt=dict(machine='simbox',command=actual_full,exit=remote_full.returncode,seconds=time.time()-full_start,log=str(full_log.relative_to(root)))
            full['attempts'].append(full_attempt);full_path.write_text(json.dumps(full,indent=2)+'\n');print(json.dumps(full_attempt),flush=True)
            if remote_full.returncode not in (0,75):raise SystemExit(remote_full.returncode)
        batch_command=['/tmp/umvl-cpu/run4',sys.executable,str(folder/'local-final-batch.py')]
        batch_start=time.time();batch=subprocess.run(batch_command,cwd=root,env=env)
        (folder/'local-final-batch-reservation.json').write_text(json.dumps(dict(machine='workstation',command=batch_command,slots=4,seconds_including_wait=time.time()-batch_start,exit=batch.returncode),indent=2)+'\n')
        raise SystemExit(batch.returncode)
    if p.returncode!=75 or machine!='simbox':break
(folder/f'{name}-run.json').write_text(json.dumps(record,indent=2)+'\n')
raise SystemExit(p.returncode)
