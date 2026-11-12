"""Check every new density regression against both native consumer binaries."""
import json,math,subprocess
from pathlib import Path
here=Path(__file__).resolve().parent;root=here.parents[3]
refs=json.loads((here/'density-references.json').read_text());result=[]
for mode in ['native','double']:
 exe=root/'build/scratch/dpq-tails-consumers'/('probe-double' if mode=='double' else 'probe')
 jobs=[(r,x,y) for r in refs for x,y in zip(r['data'],r['expected'])]
 text=''.join(' '.join(map(str,[r['family'],0,x,1,1,len(r['parameters']),*r['parameters']]))+'\n' for r,x,y in jobs)
 vals=list(map(float,subprocess.check_output([str(exe)],input=text,text=True).splitlines()))
 for (r,x,y),actual in zip(jobs,vals):
  e=abs(actual-y)/max(1,abs(y));result.append(dict(mode=mode,family=r['family'],index=r['index'],x=x,actual=actual,expected=y,error=e));assert e<=1e-10,(r,x,actual,y)
(here/'density-regressions.json').write_text(json.dumps(result,indent=2)+'\n')
print('density checks',len(result),'max normalized error',max(r['error'] for r in result))
