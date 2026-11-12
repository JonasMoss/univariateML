"""Check all seven restored natural tails, including tails below 1e-300."""
import json,subprocess
from pathlib import Path
here=Path(__file__).resolve().parent;root=here.parents[3]
refs=[r for r in json.loads((here.parent/'2026-11-11-oracle-gaps-data/row-exclusions.json').read_text()) if r['family'] in ['beta','betapr','fpareto','gb2']]
out=[]
for mode in ['native','double']:
 exe=root/'build/scratch/dpq-tails-consumers'/('probe-double' if mode=='double' else 'probe')
 text=''.join(' '.join(map(str,[r['family'],1,r['measurements'][0]['input'],int(r['measurements'][0]['lower']),0,len(r['parameters']),*r['parameters']]))+'\n' for r in refs)
 actual=list(map(float,subprocess.check_output([str(exe)],input=text,text=True).splitlines()))
 for r,a in zip(refs,actual):
  b=r['measurements'][0]['expected'];error=abs(a-b)/b
  out.append(dict(mode=mode,family=r['family'],parameters=r['parameters'],input=r['selector'][0],actual=a,expected=b,error=error));assert error<=1e-10,out[-1]
(here/'beta-regressions.json').write_text(json.dumps(out,indent=2)+'\n');print('beta tail checks',len(out),'max relative error',max(r['error'] for r in out))
