"""Four reserved workers, isolated processes, frozen binary64 replay inputs.
Invoke through run4. The serial baseline is reconstructed from commit bc8ff7be.
"""
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor
import json,subprocess,sys,time,hashlib,os
P=Path(__file__).resolve().parent;ROOT=P.parents[3]
mode=sys.argv[1];assert mode in ['native','double']
if mode=='native':
 subprocess.run([sys.executable,str(P/'check_normal.py')],check=True)
 subprocess.run([sys.executable,str(P/'check_forced_burr.py')],check=True)
 subprocess.run([sys.executable,str(P/'burr_refutation.py')],check=True)
 subprocess.run([sys.executable,str(P/'bhatt_quantization.py')],check=True)
work=ROOT/'build/scratch/acc-parallel';work.mkdir(parents=True,exist_ok=True)
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
exe=work/('fit-'+mode)
def header_digests():return {str(p.relative_to(ROOT)):hashlib.sha256(p.read_bytes()).hexdigest() for p in (ROOT/'univariateML-cpp/include').rglob('*.hpp')}
headers_before_compile=header_digests()
flags=['-O2','-std=c++17']+(['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'] if mode=='double' else [])
subprocess.run(['g++',*flags,'-I'+str(ROOT/'univariateML-cpp/include'),'-I'+str(ROOT/'bindings'),'-I'+boost,str(P/'runner.cpp'),'-o',str(exe)],check=True)
cases=json.loads((P/'cases.json').read_text());lines=[]
for c in cases:
 held={k:v for k,v in c['held'].items() if v is not None}
 row=[c['family'],str(c['index']),str(len(c['data'])),str(len(held)),str(c.get('iterlim',1000)),str(c.get('tol',1e-10))]+list(map(repr,c['data']))
 for k,v in held.items():row += [k,repr(v)]
 lines.append(' '.join(row)+'\n')
# Keep each worker's result resumable, but only reuse an exact source/input match.
sources=header_digests()
assert sources==headers_before_compile,'Headers changed during replay compilation'
fingerprint=hashlib.sha256((json.dumps(sources,sort_keys=True)+json.dumps(flags)+''.join(lines)).encode()).hexdigest()
def worker(i):
 output=work/(mode+'-'+str(i)+'.tsv');mark=work/(mode+'-'+str(i)+'.sha256')
 if mark.exists() and mark.read_text()==fingerprint:return output.read_text()
 with output.open('w') as f:subprocess.run([str(exe)],input=''.join(lines[i::4]),stdout=f,text=True,check=True)
 mark.write_text(fingerprint);return output.read_text()
start=time.monotonic()
with ThreadPoolExecutor(max_workers=4) as pool:chunks=list(pool.map(worker,range(4)))
rows={line.split('\t')[0]:line for chunk in chunks for line in chunk.splitlines()}
assert len(rows)==len(cases)
(P/('current-'+mode+'.tsv')).write_text(''.join(rows[str(c['index'])]+'\n' for c in cases))
(P/('current-'+mode+'-provenance.json')).write_text(json.dumps(dict(baseline='bc8ff7be2e3bc4b9186a56fc4f2e8ee45c163714',flags=flags,workers=4,source_input_fingerprint=fingerprint,fit_wall_seconds=time.monotonic()-start,compiler=subprocess.check_output(['g++','--version'],text=True),machine=os.uname().nodename),indent=2)+'\n')
print(mode,len(rows),'rows',time.monotonic()-start,'seconds',flush=True)
if mode=='native' and not (P/'burr-stationary-evidence.json').exists():subprocess.run([sys.executable,str(P/'burr_stationary.py')],check=True)
if mode=='double':
 for baseline in ['strict-before-native','strict-before-double']:
  subprocess.run([sys.executable,str(P/'replay.py'),baseline],check=True)
 reference_ids=[str(c['index']) for c in cases if str(c['index']).startswith(('strict-seed','strict-default'))]
 reference_environment=dict(os.environ,UMVL_REFERENCE_IDS=','.join(reference_ids))
 subprocess.run([sys.executable,str(P/'reference.py'),'current-native'],env=reference_environment,check=True)
 subprocess.run([sys.executable,str(P/'append_delegate_tests.py')],check=True)
 subprocess.run([sys.executable,str(P/'measure.py')],check=True,env=dict(os.environ,UMVL_MEASURE_BINDINGS='0'))
if mode=='double':subprocess.run([sys.executable,str(P/'burr_faces.py')],check=True)
if mode=='double' and not (P/'burr-profile-evidence.json').exists():subprocess.run([sys.executable,str(P/'burr_profile.py')],check=True)
if mode=='double' and not (P/'held-power-edge-probe.json').exists():subprocess.run([sys.executable,str(P/'held_power_probe.py')],check=True)
