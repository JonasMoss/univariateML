"""Read-only baseline identity and optimization matrix for unrelated tail failures.
Invoke through run. No distribution or shared header is changed.
"""
import hashlib,json,os,re,subprocess
from pathlib import Path
p=Path(__file__).resolve().parent;root=p.parents[3];base='bb46078757dad3538bfb263086d4e72cde163330'
os.environ['TMPDIR']=str(root/'build/tmp')
queue=[root/'univariateML-cpp/include/cppunivariate/distributions'/f/'dpqr.hpp' for f in ['burr','invburr']];seen={}
while queue:
 f=queue.pop()
 if f in seen:continue
 body=f.read_bytes();rel=str(f.relative_to(root));old=subprocess.check_output(['git','show',base+':'+rel],cwd=root);assert body==old,rel
 seen[f]=hashlib.sha256(body).hexdigest()
 for name in re.findall(r'#include "([^"]+)"',body.decode()):
  target=(f.parent/name).resolve()
  if target.is_file():queue.append(target)
rows=[]
for tag,flags in [('O0',['-O0']),('O1',['-O1']),('O2',['-O2']),('O2-no-inline',['-O2','-fno-inline']),('double-O2',['-O2','-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'])]:
 exe=root/'build/scratch'/('baseline-tail-'+tag)
 log=root/'build/scratch'/('baseline-tail-'+tag+'.log')
 with log.open('w') as out:subprocess.run(['g++','-std=c++17',*flags,'-I'+str(root/'univariateML-cpp/include'),str(p/'tail_probe.cpp'),'-o',str(exe)],stdout=out,stderr=subprocess.STDOUT,check=True)
 output=subprocess.check_output([str(exe),'1e100','1e-308','1e308'],text=True)
 rows.append(dict(flags=flags,output=output))
record=dict(baseline=base,headers_identical={str(f.relative_to(root)):h for f,h in seen.items()},matrix=rows)
(p/'baseline-tail-check.json').write_text(json.dumps(record,indent=2)+'\n');print(json.dumps(record,indent=2))
assert 'runtime -230.' in rows[0]['output'];assert 'runtime -inf -inf' in rows[2]['output']
