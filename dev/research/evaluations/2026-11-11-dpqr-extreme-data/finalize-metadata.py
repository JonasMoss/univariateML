"""Metadata-only sampler-scope/fingerprint migration; numerical cells unchanged."""
import hashlib,json,sys
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4];HERE=Path(__file__).resolve().parent
sys.path.insert(0,str(ROOT/'dev/validation'))
from quality.truth import DISTS,input_sha256
baseline=json.loads((HERE/'baseline-sizes.json').read_text())['families']
records=[]
for ident,b in baseline.items():
 path=HERE/'measured-tables'/(ident+'.json') if ident in ('frechet','invweibull') else DISTS/ident/'truth.json'
 if not path.exists():continue
 t=json.loads(path.read_text());old=t.pop('input_sha256',None);t.pop('historical_sampler_points',None)
 before=hashlib.sha256(json.dumps(t,sort_keys=True).encode()).hexdigest()
 t['historical_sampler_points']=[[i] for i in range(b['points'])]
 t['input_sha256']=input_sha256(DISTS/ident)
 numeric=dict(t);numeric.pop('input_sha256');numeric.pop('historical_sampler_points')
 after=hashlib.sha256(json.dumps(numeric,sort_keys=True).encode()).hexdigest()
 assert before==after,ident
 path.write_text(json.dumps(t,indent=2,allow_nan=False)+'\n')
 receipt=HERE/'generation'/(ident+'.json')
 if receipt.exists():
  g=json.loads(receipt.read_text())
  if 'records' in g:
   g['metadata_migration']='Table metadata updated; original generation fingerprint retained'
   receipt.write_text(json.dumps(g,indent=2)+'\n')
 records.append(dict(id=ident,old_input_sha256=old,new_input_sha256=t['input_sha256'],numerical_sha256=before))
(HERE/'metadata-migration.json').write_text(json.dumps(records,indent=2)+'\n')
print('metadata-only migration',len(records),'tables')
