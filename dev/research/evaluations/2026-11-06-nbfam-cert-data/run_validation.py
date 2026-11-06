"""Foreground CPU-reserved validation. One invocation runs one stage to completion."""
import datetime,json,os,shlex,subprocess,sys,time
from pathlib import Path
stage,machine,command=sys.argv[1:]
base=Path('dev/research/evaluations/2026-11-06-nbfam-cert-data')
log=Path('build/scratch')/(stage+'.log')
env=os.environ.copy()
for key in ['TMPDIR','JUST_TEMPDIR','XDG_RUNTIME_DIR']:env[key]=str(Path('build/tmp').resolve())
start=datetime.datetime.now(datetime.timezone.utc).isoformat();wall=time.monotonic()
payload='export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"; /usr/bin/time -f NBFAM_COMPUTE_SECONDS=%e bash -c '+shlex.quote(command)
attempts=[]
while True:
    launcher='/tmp/umvl-cpu/simbox-run' if machine=='simbox' else '/tmp/umvl-cpu/run' if machine=='local1' else '/tmp/umvl-cpu/run4'
    with log.open('a') as f:
        f.write(f'COMMAND {command}\nMACHINE {machine}\n');f.flush()
        run=subprocess.run([launcher,'bash','-c',payload],stdout=f,stderr=subprocess.STDOUT,env=env)
    attempts.append(dict(machine=machine,exit=run.returncode))
    if run.returncode==75 and machine=='simbox':machine='local';continue
    break
content=log.read_text(errors='replace');seconds=[line.split('=',1)[1] for line in content.splitlines() if line.startswith('NBFAM_COMPUTE_SECONDS=')]
record=dict(command=command,machine=machine,started_utc=start,wall_including_slot_wait_seconds=time.monotonic()-wall,
            compute_seconds=float(seconds[-1]) if seconds else None,attempts=attempts,exit=run.returncode)
(base/(stage+'.json')).write_text(json.dumps(record,indent=2)+'\n')
print(json.dumps(record,indent=2));print('\n'.join(content.splitlines()[-35:]))
raise SystemExit(run.returncode)
