from pathlib import Path
import subprocess,json,time,os,hashlib
root=Path.cwd();p=root/'dev/research/evaluations/2026-11-11-acc-fb-cont-data';scratch=root/'build/scratch'
results=[]
def hashes():return {str(f.relative_to(root)):hashlib.sha256(f.read_bytes()).hexdigest() for f in list((root/'univariateML-cpp/include').rglob('*.hpp'))+list((root/'univariateML-cpp/include').rglob('README.md'))}
initial=hashes()
def run(name,cmd,output=None,env=None):
 start=time.monotonic()
 with (output or scratch/(name+'-final.log')).open('w') as log:r=subprocess.run(cmd,stdout=log,stderr=subprocess.STDOUT,env=env)
 results.append(dict(check=name,command=cmd,exit_code=r.returncode,wall_seconds=time.monotonic()-start,machine=os.uname().nodename))
 (p/'final-js-site-checks.json').write_text(json.dumps(dict(checks=results,sha256=initial),indent=2)+'\n');print(name,r.returncode,flush=True)
run('js-build',['just','js-build'])
run('js-test',['just','js-test'])
run('site-check',['just','site-check'])
image='dev/research/evaluations/2026-11-02-viz-clip/after-mobile-held-face.png'
(root/image).write_bytes(subprocess.check_output(['git','show','bc8ff7be:'+image]))
run('wasm-spots',['node','--v8-pool-size=1',str(p/'spot.mjs')],p/'wasm-spots.jsonl')
run('wasm-measure',['python3',str(p/'measure.py')],env=dict(os.environ,UMVL_MEASURE_AFTER='strict-probe',UMVL_MEASURE_OUTPUT='wasm-final-',UMVL_MEASURE_BINDINGS='wasm'))
assert hashes()==initial,'Source changed during final JS/site checks'
