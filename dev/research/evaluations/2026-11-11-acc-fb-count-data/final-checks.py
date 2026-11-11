"""Blocking workstation final checks. Invoke through /tmp/umvl-cpu/run4."""
import datetime,hashlib,json,os,pathlib,subprocess,sys,time
P=pathlib.Path(__file__).resolve().parent
LOG=pathlib.Path('build/scratch/acc-fb-count-logs')
LOG.mkdir(parents=True,exist_ok=True)
commands=sys.argv[1:] or ['cpp-test','cpp-test-no-long-double','test','test-full','js-build','js-test','site-check']
record=P/'final-checks.json'
rows=json.loads(record.read_text()) if record.exists() else {}
def fingerprint():
 paths=[]
 for family in ['bbinom','hyper','nhyper','tbinom','ztbinom','zib','zmbinom','bnbinom','skellam']:
  for base in ['univariateML-cpp/include/cppunivariate/distributions','bindings/common/distributions','bindings/r/distributions']:
   paths.extend(pathlib.Path(base,family).glob('*'))
 paths.append(pathlib.Path('bindings/js/stop-test.mjs'))
 digest=hashlib.sha256()
 for path in sorted(p for p in paths if p.is_file() and p.suffix in ['.hpp','.cpp','.R','.md','.mjs']):
  digest.update(str(path).encode());digest.update(path.read_bytes())
 return digest.hexdigest()
for target in commands:
 source_hash=fingerprint()
 log=LOG/f'{target}.log';start=time.monotonic();stamp=datetime.datetime.now(datetime.timezone.utc).isoformat()
 print(target,'START',stamp,flush=True)
 with log.open('w') as stream:r=subprocess.run(['just',target],stdout=stream,stderr=subprocess.STDOUT)
 rows[target]=dict(machine='workstation',command='just '+target,exit_code=r.returncode,seconds=time.monotonic()-start,started=stamp,log=str(log),source_sha256=source_hash)
 record.write_text(json.dumps(rows,indent=2)+'\n')
 print(target,'END',r.returncode,round(rows[target]['seconds'],1),'seconds',flush=True)
 if r.returncode:print('FAILURE TAIL\n'+'\n'.join(log.read_text(errors='replace').splitlines()[-35:]),flush=True)
sys.exit(any(rows[t]['exit_code'] for t in commands))
