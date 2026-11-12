"""Rebuild WASM and repeat the full suite, preserving the completed review checks.

Run only after review_validate.py completes, through the local run4 wrapper.
The initial review driver omitted the separate js-build recipe; its retained
first JS result is superseded by this required build and subsequent full run.
"""
from pathlib import Path
import hashlib,json,os,subprocess,time
root=Path.cwd();p=root/'dev/research/evaluations/2026-11-11-acc-fb-cont-data';work=root/'build/scratch/review-round'
manifest=json.loads((p/'review-validation.json').read_text())
# Retain the failed measurement attempt separately: R JSON numeric formatting
# shortened held sd before its exact-value comparison. No fit or test failed.
if any(c['exit_code']!=0 for c in manifest['checks']):
 assert [c['check'] for c in manifest['checks'] if c['exit_code']!=0]==['ged-reference-and-bindings']
 (p/'review-validation-before-ged-serialization-fix.json').write_text(json.dumps(manifest,indent=2)+'\n')
 (work/'validation-before-ged-serialization-fix.log').write_bytes((work/'validation.log').read_bytes())
 manifest['checks']=[c for c in manifest['checks'] if c['exit_code']==0]
assert all(c['exit_code']==0 for c in manifest['checks'])
start=json.loads((p/'review-start.json').read_text());allowed=set(start['authorized_test_files'])
frozen={name:hashlib.sha256((root/name).read_bytes()).hexdigest() for name in allowed}
def guard():
 for name,expected in start['sha256'].items():
  assert name in allowed or hashlib.sha256((root/name).read_bytes()).hexdigest()==expected,name
 for name,expected in frozen.items():assert hashlib.sha256((root/name).read_bytes()).hexdigest()==expected,name
guard()
(p/'review-validation-before-js-rebuild.json').write_text(json.dumps(manifest,indent=2)+'\n')
(work/'js-before-rebuild.log').write_bytes((work/'js-full.log').read_bytes())
os.environ.update(OPENBLAS_NUM_THREADS='1',OMP_NUM_THREADS='1',MKL_NUM_THREADS='1',CMAKE_BUILD_PARALLEL_LEVEL='4',CTEST_PARALLEL_LEVEL='4')
for name,args in [('js-build',['just','js-build']),('js-full',['just','js-test']),('ged-reference-and-bindings',['python3',str(p/'review_ged_measure.py')]),('scope-and-input-audit',['python3',str(p/'review_audit.py')])]:
 guard();now=time.monotonic()
 with (work/(name+'.log')).open('w') as output:r=subprocess.run(args,stdout=output,stderr=subprocess.STDOUT)
 manifest['checks'].append(dict(check=name,command=args,exit_code=r.returncode,wall_seconds=time.monotonic()-now,machine=os.uname().nodename));guard()
 (p/'review-validation.json').write_text(json.dumps(manifest,indent=2)+'\n');print(name,r.returncode,flush=True)
 assert r.returncode==0,(name,r.returncode)
print('ALL JS REBUILD CHECKS FINISHED',flush=True)
with (work/'validation.log').open('a') as output:output.write('\nALL REVIEW CHECKS FINISHED\n')
