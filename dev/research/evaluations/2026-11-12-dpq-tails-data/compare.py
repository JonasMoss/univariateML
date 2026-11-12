"""Compare all archive/golden public fit outcomes, coefficients and logLik."""
import json
from pathlib import Path
here=Path(__file__).resolve().parent
read=lambda name:{(r['family'],r['source'],r['index']):r for r in map(json.loads,(here/name).read_text().splitlines())}
before=read('before.jsonl');after=read('final.jsonl');assert before.keys()==after.keys()
changes=[];allowed=[];count=0;opens=[]
retry_path=here/'baseline-retries.json'
if retry_path.exists():
 for retry in json.loads(retry_path.read_text()):
  key=('fpareto','archive',retry['index'])
  if before[key]['outcome']!='open fit' or 'stdout' not in retry:continue
  cells=retry['stdout'].strip().split('\t')
  if cells[0]=='ok':
   before[key].pop('reason',None)
   before[key].update(outcome='pass',reported=float(cells[1]),parameters=list(map(float,cells[2:7])))
  elif cells[0]=='stop':before[key].update(outcome='stop',reason='\t'.join(cells[1:]))
for k,b in before.items():
 a=after[k]
 if b['outcome']=='open fit' or a['outcome']=='open fit':opens.append(dict(key=k,before=b['outcome'],after=a['outcome']));continue
 for field in ['parameters','reported','reason']:
  if a.get(field)!=b.get(field):
   entry=dict(key=k,field=field,before=b.get(field),after=a.get(field))
   if k[0]=='dgamma' and k[2] in [1025,1027] and field=='reported':allowed.append(entry)
   else:changes.append(entry)
 if bool('parameters' in a)!=bool('parameters' in b):changes.append(dict(key=k,field='successful status'))
 count+=1
result=dict(rows=len(before),compared=count,changes=changes,allowed_likelihood_changes=allowed,opens=opens,final_mismatches=[k for k,a in after.items() if a['outcome']=='mismatch'])
(here/'fit-comparison.json').write_text(json.dumps(result,indent=2)+'\n');print(result)
assert not changes
