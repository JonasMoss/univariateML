"""Finish validation after correcting only the new Bhatt fixture control name.
All fitter headers, tests, dossiers and compiled modules remain unchanged.
The archived manifest retains the original failed measurement.
"""
from pathlib import Path
import subprocess,os,json,time,hashlib
root=Path.cwd();p=root/'dev/research/evaluations/2026-11-11-acc-fb-cont-data';s=root/'build/scratch'
manifest=json.loads((p/'complete-validation.json').read_text());js=json.loads((p/'final-js-site-checks.json').read_text())
def guard():
 for name,expected in manifest['sha256'].items():assert hashlib.sha256((root/name).read_bytes()).hexdigest()==expected,name
def run(name,cmd,output=None,env=None,child=False):
 guard();start=time.monotonic()
 with (output or s/(name+'.log')).open('w') as log:r=subprocess.run(cmd,stdout=log,stderr=subprocess.STDOUT,env=env)
 record=dict(check=name,command=cmd,exit_code=r.returncode,wall_seconds=time.monotonic()-start,machine=os.uname().nodename,rerun_reason='Corrected only the new Bhatt fixtures from gradtol to the existing reltol argument.')
 target=js if child else manifest;old=[x for x in target['checks'] if x['check']==name];target.setdefault('superseded_checks',[]).extend(old)
 target['checks']=[record if x['check']==name else x for x in target['checks']]
 if not old:target['checks'].append(record)
 (p/('final-js-site-checks.json' if child else 'complete-validation.json')).write_text(json.dumps(target,indent=2)+'\n');print(name,r.returncode,flush=True);guard();assert r.returncode==0,name
run('r-spots-complete',['Rscript',str(p/'spot.R')],p/'r-spots.jsonl')
run('wasm-spots',['node','--v8-pool-size=1',str(p/'spot.mjs')],p/'wasm-spots.jsonl',child=True)
run('wasm-measure',['python3',str(p/'measure.py')],env=dict(os.environ,UMVL_MEASURE_AFTER='strict-probe',UMVL_MEASURE_OUTPUT='wasm-final-',UMVL_MEASURE_BINDINGS='wasm'),child=True)
run('measure-complete',['python3',str(p/'measure.py')])
run('integrity-complete',['python3',str(p/'integrity.py')])
manifest['binding_fixture_correction']='Fourteen new Bhatt fixtures used gradtol; corrected to existing reltol. Only R/WASM spots and measurements were repeated; C++ inputs and all fitter/test/dossier source hashes stayed unchanged.'
(p/'complete-validation.json').write_text(json.dumps(manifest,indent=2)+'\n')
print('ALL FINAL COMMANDS FINISHED',flush=True)
