"""Replay fit outcomes, coefficients and logLik in double-width mode.
Baseline sources are reconstructed in ignored scratch, never archived as evidence.
"""
import importlib.util,subprocess,json,tarfile,io,sys,hashlib,math
from pathlib import Path
HERE=Path(__file__).resolve().parent; ROOT=HERE.parents[3]
sys.path.insert(0,str(ROOT/'dev/validation'))
spec=importlib.util.spec_from_file_location('seam',ROOT/'dev/validation/seam-check.py')
s=importlib.util.module_from_spec(spec);spec.loader.exec_module(s)
BASE='217ab8b533cac25ccc8eba8d66eccde1b376f0b4'
work=ROOT/'build/scratch/dpq-counts-fit-double';work.mkdir(exist_ok=True)
baseline=work/'baseline';baseline.mkdir(exist_ok=True)
blob=subprocess.check_output(['git','archive',BASE,'univariateML-cpp/include','bindings/common'])
with tarfile.open(fileobj=io.BytesIO(blob)) as archive:archive.extractall(baseline,filter='data')
families=['bnbinom','cmp','lgser','pig','skellam']
s.WORK=work
frozen_cases=list(s.cases(families))
results={}
for label,source in [('before',baseline),('after',ROOT)]:
 s.ROOT=source
 exe=s.build(families,True,label)
 records=[]
 for case in frozen_cases:
  ident=case['family'];meta,_=s.read(s.DISTS/ident);names=[p['name'] for p in meta['params']]
  holds=[float(case['held'].get(n,0))for n in names];mask=sum(1<<i for i,n in enumerate(names)if n in case['held'])
  line=' '.join(map(str,[ident,mask,len(names),len(case['data']),*holds,*case['data']]))+'\n'
  r=dict(family=ident,index=case['index'],source=case['source'])
  try:
   output=subprocess.run([str(exe)],input=line,text=True,capture_output=True,check=True,timeout=120).stdout.strip().split('\t')
   if output[0]=='ok':r.update(status='Ok',logLik=float(output[1]),coefficients=list(map(float,output[2:2+len(names)])))
   else:r.update(status='stop',reason='\t'.join(output[1:]))
  except Exception as error:r.update(status='audit error',reason=str(error))
  records.append(r)
 (HERE/f'fits-{label}-double.json').write_text(json.dumps(records,indent=2)+'\n')
 results[label]=records
 print(label,len(records),'done',flush=True)
changes=[]
for old,new in zip(results['before'],results['after']):
 if old!=new:changes.append(dict(before=old,after=new))
(HERE/'fit-preservation-double.json').write_text(json.dumps(dict(records=len(results['after']),baseline=BASE,changes=changes),indent=2)+'\n')
print('changes',len(changes),flush=True)
