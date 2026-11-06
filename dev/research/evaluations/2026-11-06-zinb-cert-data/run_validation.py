"""Run one foreground validation command and retain its status and elapsed time."""
import datetime,json,os,subprocess,sys,time
from pathlib import Path
root=Path(__file__).resolve().parents[4]
os.chdir(root)
for key in ('TMPDIR','JUST_TEMPDIR','XDG_RUNTIME_DIR'):os.environ[key]=str(root/'build/tmp')
(root/'build/tmp').mkdir(parents=True,exist_ok=True)
name,machine,*command=sys.argv[1:]
base=Path(__file__).resolve().parent
start=time.monotonic();when=datetime.datetime.now(datetime.timezone.utc).isoformat()
with (base/(name+'.txt')).open('w') as log:
 result=subprocess.run(command,stdout=log,stderr=subprocess.STDOUT)
record=dict(command=command,machine=machine,started=when,elapsed_seconds=time.monotonic()-start,exit_code=result.returncode)
(base/(name+'-run.json')).write_text(json.dumps(record,indent=2)+'\n')
print(json.dumps(record),flush=True)
raise SystemExit(result.returncode)
