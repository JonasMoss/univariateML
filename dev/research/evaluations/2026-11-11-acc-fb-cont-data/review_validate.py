"""Reviewer-authorized expectations, five families in both C++ modes and R, full JS.
Run through /tmp/umvl-cpu/run4. All children are blocking foreground commands.
"""
from pathlib import Path
import subprocess,json,os,time,hashlib
root=Path.cwd();p=root/'dev/research/evaluations/2026-11-11-acc-fb-cont-data';work=root/'build/scratch/review-round';work.mkdir(parents=True,exist_ok=True)
os.environ.update(CCACHE_DIR=os.environ.get('UMVL_CCACHE_DIR',os.environ['HOME']+'/.cache/ccache/univariateML'),CCACHE_BASEDIR=str(root),CCACHE_NOHASHDIR='1',CCACHE_MAXSIZE='8G',CMAKE_BUILD_PARALLEL_LEVEL='4',CTEST_PARALLEL_LEVEL='4',OPENBLAS_NUM_THREADS='1',OMP_NUM_THREADS='1',MKL_NUM_THREADS='1',TESTTHAT_CPUS='4',UMVL_TEST_TIER='full',UMVL_DISTRIBUTIONS_DIR=str(root/'univariateML-cpp/include/cppunivariate/distributions'))
start=json.loads((p/'review-start.json').read_text());allowed=set(start['authorized_test_files']);checks=[]
frozen={name:hashlib.sha256((root/name).read_bytes()).hexdigest() for name in allowed}
def guard():
 changed=[]
 for name,expected in start['sha256'].items():
  if hashlib.sha256((root/name).read_bytes()).hexdigest()!=expected:changed.append(name)
 assert set(changed)<=allowed,changed
 for name,expected in frozen.items():assert hashlib.sha256((root/name).read_bytes()).hexdigest()==expected,("Test changed during validation",name)
 return changed
def run(name,args,env=None):
 before=guard();now=time.monotonic()
 with (work/(name+'.log')).open('w') as output:r=subprocess.run(args,stdout=output,stderr=subprocess.STDOUT,env=env)
 checks.append(dict(check=name,command=args,exit_code=r.returncode,wall_seconds=time.monotonic()-now,machine=os.uname().nodename));guard()
 (p/'review-validation.json').write_text(json.dumps(dict(checks=checks,authorized_test_changes=before,production_source_unchanged=True),indent=2)+'\n');print(name,r.returncode,flush=True)
 assert r.returncode==0,(name,r.returncode)
for mode,directory in [('native','univariateML-cpp/build'),('double','build/cpp-no-long-double')]:
 run('cpp-build-'+mode,['cmake','--build',directory,'--target','distribution_tests','-j4'])
 run('cpp-families-'+mode,['ctest','--test-dir',directory,'--output-on-failure','-j4','-R','^distribution_(burr|invburr|pareto4|bhatt|ged)(_|$)'])
run('r-regen',['just','regen'])
run('r-families',['Rscript','-e','options(pkg.build_extra_flags=FALSE); testthat::test_local("univariateML-r", filter="^dist-(burr|invburr|pareto4|bhatt|ged)$", stop_on_failure=TRUE)'])
run('js-build',['just','js-build'])
run('js-full',['just','js-test'])
run('ged-reference-and-bindings',['python3',str(p/'review_ged_measure.py')])
run('scope-and-input-audit',['python3',str(p/'review_audit.py')])
print('ALL REVIEW CHECKS FINISHED',flush=True)
