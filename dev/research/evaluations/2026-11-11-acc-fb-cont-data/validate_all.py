from pathlib import Path
import subprocess,os,json,time,hashlib
root=Path.cwd();p=root/'dev/research/evaluations/2026-11-11-acc-fb-cont-data';s=root/'build/scratch';checks=[]
def hashes():return {str(f.relative_to(root)):hashlib.sha256(f.read_bytes()).hexdigest() for f in list((root/'univariateML-cpp/include').rglob('*.hpp'))+list((root/'univariateML-cpp/include').rglob('test.cpp'))+list((root/'univariateML-cpp/include').rglob('README.md'))}
s.mkdir(parents=True,exist_ok=True)
baseline=root/'build/scratch/acc-baseline'
if not (baseline/'univariateML-cpp/include/cppunivariate/distributions/burr/mle.hpp').exists():
 baseline.mkdir(parents=True,exist_ok=True)
 archive=subprocess.Popen(['git','archive','bc8ff7be2e3bc4b9186a56fc4f2e8ee45c163714','univariateML-cpp'],stdout=subprocess.PIPE)
 subprocess.run(['tar','-x','-C',str(baseline)],stdin=archive.stdout,check=True)
 archive.stdout.close();assert archive.wait()==0
initial=hashes()
def run(name,args,env=None,output=None,required=False):
 start=time.monotonic()
 with (output or s/(name+'.log')).open('w') as log:r=subprocess.run(args,stdout=log,stderr=subprocess.STDOUT,env=env)
 checks.append(dict(check=name,command=args,exit_code=r.returncode,wall_seconds=time.monotonic()-start,machine=os.uname().nodename))
 (p/'complete-validation.json').write_text(json.dumps(dict(checks=checks,sha256=initial),indent=2)+'\n');print(name,r.returncode,flush=True)
 assert hashes()==initial,'Source changed during final validation'
 if required and r.returncode:raise SystemExit('Targeted validation failed: '+name)
 return r.returncode
run('accuracy-focused-final',['python3',str(p/'focused_tests.py')],required=True)
run('bhatt-weak-reference-final',['python3',str(p/'bhatt_weak_reference.py')],required=True)
run('bhatt-held-sigma-reference-final',['python3',str(p/'bhatt_held_sigma_reference.py')],required=True)
for mode in ['strict-before-native','strict-before-double','strict-probe-native','strict-probe-double']:run(mode+'-final',['python3',str(p/'replay.py'),mode],required=True)
keys=[str(c['index']) for c in json.loads((p/'cases.json').read_text()) if str(c['index']).startswith(('strict-seed','strict-default'))]
run('strict-reference-final',['python3',str(p/'reference.py'),'strict-probe-native'],dict(os.environ,UMVL_REFERENCE_IDS=','.join(keys)),required=True)
run('strict-measure-final',['python3',str(p/'measure.py')],dict(os.environ,UMVL_MEASURE_AFTER='strict-probe',UMVL_MEASURE_OUTPUT='targeted-',UMVL_MEASURE_BINDINGS='0'),required=True)
run('betapr-seam-final',['python3',str(p/'betapr_seam_probe.py')],required=True)
run('coupled-burr-final',['python3',str(p/'coupled_burr_probe.py')],required=True)
run('comparable-beta-final',['python3',str(p/'comparable_beta_probe.py')],required=True)
run('bhatt-weak-final',['python3',str(p/'bhatt_weak_probe.py')],required=True)
run('betapr-fixed-scale-supplemental',['python3',str(p/'betapr_fixed_scale_probe.py')],required=True)
# Refresh dependencies once so no old CMake object escapes this validation.
changed=subprocess.check_output(['git','diff','--name-only'],text=True).splitlines()+subprocess.check_output(['git','ls-files','--others','--exclude-standard'],text=True).splitlines()
for name in changed:
 f=root/name
 if f.suffix=='.hpp' and name.startswith('univariateML-cpp/include/'):os.utime(f,None)
run('cpp-dev-complete',['just','cpp-test'])
run('cpp-double-complete',['just','cpp-test-no-long-double'])
for mode in ['native','double']:run('replay-complete-'+mode,['python3',str(p/'parallel_replay.py'),mode],required=True)
run('r-dev-complete',['just','test'])
run('r-full-complete',['just','test-full'])
run('r-spots-complete',['Rscript',str(p/'spot.R')],output=p/'r-spots.jsonl',required=True)
run('js-site-complete',['python3',str(p/'validate_bindings_site.py')],required=True)
run('measure-complete',['python3',str(p/'measure.py')],required=True)
run('integrity-complete',['python3',str(p/'integrity.py')],required=True)
print('ALL FINAL COMMANDS FINISHED',flush=True)
