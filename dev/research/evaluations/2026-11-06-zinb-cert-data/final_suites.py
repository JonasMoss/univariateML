"""Sequential foreground suites; simbox loss (75) retries with local reservations."""
import os,subprocess,sys
from pathlib import Path
root=Path(__file__).resolve().parents[4]
os.chdir(root)
runner=str(Path(__file__).with_name('run_validation.py'))
suites={
 'cpp-dev':['just','cpp-test'],
 'cpp-no-long-double':['just','cpp-test-no-long-double'],
 'cpp-full':['env','UMVL_TEST_TIER=full','just','cpp-test'],
 'r-dev':['just','test'],
 'r-full':['just','test-full'],
 'js-build':['just','js-build'],
 'js-test':['just','js-test'],
 'site-check':['just','site-check'],
}
for name in sys.argv[1:]:
 command=suites[name]
 if name.startswith(('cpp','r-')):
  result=subprocess.run(['python3',runner,name,'simbox','/tmp/umvl-cpu/simbox-run']+command)
  if result.returncode==75:
   result=subprocess.run(['python3',runner,name+'-local','workstation','/tmp/umvl-cpu/run4']+command)
 else:
  result=subprocess.run(['python3',runner,name,'workstation','/tmp/umvl-cpu/run4']+command)
 if result.returncode:raise SystemExit(result.returncode)
