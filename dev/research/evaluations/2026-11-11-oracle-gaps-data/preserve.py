"""Check append-only numerical payloads against the job's baseline commit."""
import json, subprocess, sys
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4]
sys.path.insert(0,str(ROOT/'dev/validation'))
from quality.truth import DISTS,input_sha256
BASELINE='7825e912'
records=[]
for path in sorted(DISTS.glob('*/truth.json')):
    relative=path.relative_to(ROOT)
    old=json.loads(subprocess.check_output(['git','show',f'{BASELINE}:{relative}'],cwd=ROOT,text=True))
    new=json.loads(path.read_text())
    for key in ('parameters','values','quantiles','normalization','excluded_values','excluded_quantiles','excluded_parameter_points'):
        before=old.get(key,[]);after=new.get(key,[])
        assert after[:len(before)]==before,(path.parent.name,key)
    for key in ('sampler_points','historical_sampler_points'):
        assert old.get(key)==new.get(key),(path.parent.name,key)
    if path.parent.name!='gengamma':
        new['input_sha256']=input_sha256(path.parent)
        path.write_text(json.dumps(new,indent=2,allow_nan=False)+'\n')
    records.append(dict(family=path.parent.name,before_points=len(old['parameters']),after_points=len(new['parameters']),added_values=len(new['values'])-len(old['values']),added_quantiles=len(new['quantiles'])-len(old['quantiles'])))
(ROOT/'dev/research/evaluations/2026-11-11-oracle-gaps-data/preservation.json').write_text(json.dumps(records,indent=2)+'\n')
print('Historical rows, exclusions and sampler selectors preserved:',len(records))
