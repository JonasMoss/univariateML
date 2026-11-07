"""Foreground remote replay plus immediate collection before the next mirror."""
import subprocess,json,time,os
from pathlib import Path
P=Path(__file__).resolve().parent;rel=P.relative_to(Path.cwd())
program='import runpy,sys; from pathlib import Path; p=Path('+repr(str(rel))+'); sys.path.insert(0,str(p)); '+ '; '.join('runpy.run_path(str(p/'+repr(name)+'),run_name="__main__")' for name in ['explore.py','search_modes.py','curvature_search.py','recheck_curvature.py','near_face.py','certify_near.py'])
start=time.monotonic()
r=subprocess.run(['/tmp/umvl-cpu/simbox-run','python3','-u','-c',program],stdout=subprocess.PIPE,stderr=subprocess.STDOUT,text=True)
(P/'research-final.txt').write_text(r.stdout);print(r.stdout,flush=True)
if r.returncode==75:
 r=subprocess.run(['/tmp/umvl-cpu/run','python3','-u','-c',program],stdout=subprocess.PIPE,stderr=subprocess.STDOUT,text=True)
 (P/'research-final.txt').write_text(r.stdout);print(r.stdout,flush=True);machine='workstation'
else:machine='simbox'
if r.returncode:raise SystemExit(r.returncode)
if machine=='simbox':
 for name in ['explore.json','search-modes.json','curvature-search.json','curvature-recheck.json','near-face.json','near-certificate.json','near-cover.json']:
  z=subprocess.run(['ssh','-F',str(Path.home()/'.ssh/config'),'-o','BatchMode=yes','simbox','cat umvl-wt/zinb-theory/'+str(rel/name)],capture_output=True,check=True)
  json.loads(z.stdout);(P/name).write_bytes(z.stdout)
(P/'research-run.json').write_text(json.dumps(dict(machine=machine,exit_code=r.returncode,wall_seconds=time.monotonic()-start,foreground=True,commands=['explore.py','search_modes.py','curvature_search.py','recheck_curvature.py','near_face.py','certify_near.py']),indent=2)+'\n')
print('Completed and retrieved on',machine,flush=True)
