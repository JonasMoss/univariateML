"""Blocking final suites; exit 75 from simbox falls back to local run4."""
import datetime,gzip,json,os,subprocess,time
from pathlib import Path
here=Path(__file__).resolve().parent;root=here.parents[3];os.chdir(root)
for key in ['TMPDIR','JUST_TEMPDIR','XDG_RUNTIME_DIR']:os.environ[key]=str(root/'build/tmp')
logs=root/'build/scratch/dpq-tails-logs';logs.mkdir(parents=True,exist_ok=True)
kept=here/'validation-logs';kept.mkdir(exist_ok=True)
runs=[]
for name,remote,args in [
 ('cpp-dev',True,['just','cpp-test']),
 ('cpp-full',True,['env','UMVL_TEST_TIER=full','just','cpp-test']),
 ('cpp-double-simbox',True,['just','cpp-test-no-long-double']),
 ('cpp-double-workstation',False,['just','cpp-test-no-long-double']),
 ('r-dev',True,['just','test']),('r-full',True,['just','test-full']),
 ('js-build',False,['just','js-build']),('js-test',False,['just','js-test']),
 ('site-check',False,['just','site-check'])]:
 start=time.monotonic();machine='simbox' if remote else 'workstation'
 command=['/tmp/umvl-cpu/simbox-run','env','CMAKE_BUILD_PARALLEL_LEVEL=4','CTEST_PARALLEL_LEVEL=4',*args] if remote else ['/tmp/umvl-cpu/run4','nice','-n','10',*args]
 print('START',name,machine,flush=True)
 path=logs/(name+'.log')
 with path.open('w') as out:
  result=subprocess.run(command,stdout=out,stderr=subprocess.STDOUT)
  if remote and result.returncode==75:
   machine='workstation (simbox exit 75)';out.write('\nLOCAL FALLBACK\n');out.flush()
   result=subprocess.run(['/tmp/umvl-cpu/run4','nice','-n','10',*args],stdout=out,stderr=subprocess.STDOUT)
 with gzip.open(kept/(name+'.log.gz'),'wb') as dest:dest.write(path.read_bytes())
 runs.append(dict(name=name,args=args,machine=machine,returncode=result.returncode,seconds=time.monotonic()-start))
 (here/'suites.json').write_text(json.dumps(runs,indent=2)+'\n')
 print('END',name,runs[-1],flush=True)
 if result.returncode:raise SystemExit(result.returncode)
