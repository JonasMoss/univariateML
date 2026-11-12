"""Replay unchanged truth rows, including archived members, in both modes.
Uses the frozen oracle-gaps harness; writes only this job's receipts.
"""
import importlib.util,sys,subprocess,os,json
from pathlib import Path
HERE=Path(__file__).resolve().parent
ROOT=HERE.parents[3]
sys.path.insert(0,str(ROOT/'dev/validation'))
spec=importlib.util.spec_from_file_location('audit',HERE.parent/'2026-11-11-oracle-gaps-data/audit.py')
a=importlib.util.module_from_spec(spec);spec.loader.exec_module(a)
original=a.adapters
selection=os.environ.get('UMVL_COUNTS_FAMILIES','bnbinom,cmp,lgser,pig,skellam,zmlogarithmic').split(',')
a.adapters=lambda:{k:v for k,v in original().items() if k in selection}
suffix='-'+selection[0] if len(selection)==1 else ''
a.HERE=HERE
a.WORK=ROOT/('build/scratch/dpq-counts'+suffix);a.WORK.mkdir(exist_ok=True)
if sys.argv[1]=='build':
 a.EXE=a.WORK/'probe'
 sys.argv=['audit','build','current',*(['no-long-double'] if '--double' in sys.argv else [])]
 a.build()
else:
 mode='double' if '--double' in sys.argv else 'native'
 a.EXE=a.WORK/('probe-double' if mode=='double' else 'probe')
 sys.argv=['audit','check']+(['--active-only'] if '--active-only' in sys.argv else [])
 a.check('counts'+suffix+'-'+mode+('-active' if '--active-only' in sys.argv else ''))
 if '--active-only' not in sys.argv:
  import math
  jobs=[];refs=[]
  for family in a.adapters():
   table=json.loads((a.DISTS/family/'truth.json').read_text())
   members=[(table['parameters'],table['values']+[r['row'] for r in table.get('excluded_values',[])])]
   members += [([p['parameters']],p['values']) for p in table.get('excluded_parameter_points',[])]
   for parameters,rows in members:
    for row in rows:
     ref=math.exp(float(row[2])) if float(row[2])<710 else math.inf
     if ref>=1e-300 and math.isfinite(ref):
      jobs.append((family,0,row[1],True,False,parameters[row[0]]));refs.append(ref)
  actual=a.evaluate(jobs)
  failures=[dict(job=job,actual=value,expected=ref,error=abs(value-ref)/ref if math.isfinite(value) else math.inf) for job,value,ref in zip(jobs,actual,refs) if not math.isfinite(value) or abs(value-ref)>1e-10*ref]
  (HERE/f'density{suffix}-{mode}.json').write_text(json.dumps(dict(checks=len(jobs),failures=failures),indent=2)+'\n')
