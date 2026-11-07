from pathlib import Path
import datetime,json,os,subprocess,sys,time,hashlib
base=Path(__file__).resolve().parent
name,machine,*command=sys.argv[1:]
env=os.environ.copy();root=Path.cwd();tmp=root/'build/tmp';tmp.mkdir(parents=True,exist_ok=True)
for k in ['TMPDIR','JUST_TEMPDIR','XDG_RUNTIME_DIR']:env[k]=str(tmp)
tracked=[Path(p) for p in subprocess.check_output(['git','diff','--name-only'],text=True).splitlines() if not p.endswith('.png')]
tracked += [Path('univariateML-cpp/include/cppunivariate/core/work_budget.hpp')]
manifest=''.join(f'{hashlib.sha256(p.read_bytes()).hexdigest()}  {p}\n' for p in sorted(tracked) if p.is_file())
source_sha256=hashlib.sha256(manifest.encode()).hexdigest()
headers=sorted([*Path('univariateML-cpp/include').rglob('*.hpp'),*Path('bindings/common').rglob('*.hpp')])
header_manifest=''.join(f'{hashlib.sha256(p.read_bytes()).hexdigest()}  {p}\n' for p in headers)
source_headers_sha256=hashlib.sha256(header_manifest.encode()).hexdigest()
start=time.time()
with (base/(name+'.log')).open('w') as log:
 single=machine=='workstation' and (command in [['just','js-test'],['just','site-check'],['just','gen']] or name.startswith('nbfam-') or name=='js-build-final10')
 runner='/tmp/umvl-cpu/simbox-run' if machine=='simbox' else '/tmp/umvl-cpu/run' if single else '/tmp/umvl-cpu/run4'
 launched=['python3','dev/research/evaluations/2026-11-06-nbfam-cert-data/single_core.py',*command] if single else command
 p=subprocess.run([runner,*launched],stdout=log,stderr=subprocess.STDOUT,env=env)
 if p.returncode==75 and machine=='simbox':
  machine='workstation (exit-75 fallback)';p=subprocess.run(['/tmp/umvl-cpu/run4',*command],stdout=log,stderr=subprocess.STDOUT,env=env)
result=dict(source_sha256=source_sha256,source_headers_sha256=source_headers_sha256,runner=runner,command=command,machine=machine,returncode=p.returncode,wall_seconds=time.time()-start,started_utc=datetime.datetime.fromtimestamp(start,datetime.timezone.utc).isoformat(),revision=subprocess.check_output(['git','rev-parse','HEAD'],text=True).strip())
(base/(name+'.json')).write_text(json.dumps(result,indent=2)+'\n');print(json.dumps(result));sys.exit(p.returncode)
