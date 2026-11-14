#!/usr/bin/env python3
from pathlib import Path
import json,subprocess,collections
ROOT=Path(__file__).resolve().parents[4];D=Path(__file__).parent
F=set('bbinom bhatt cauchy dgamma dunif expweibull fpareto ged gev gumbel ht invburr invgauss invtrgamma lgamma logis lst nsbeta pareto2 pig sged slash sstd tlambda unif'.split())
old=json.loads(subprocess.check_output(['git','show','HEAD:dev/validation/strict-known.json'],cwd=ROOT))['known']
path=ROOT/'dev/validation/strict-known.json';document=json.loads(path.read_text());new=document['known']
assert not (new.keys()-old.keys()),'No new known entries allowed'
# A full seam --prune can visit families and cards outside this assignment.
# Preserve their baseline listings; only the assigned repairs may leave.
protected={k:v for k,v in old.items() if k not in new and
 (k.split('|')[2] not in F or v['ref'] not in ['TASK-214.15','TASK-214.16','TASK-214.17','TASK-214.18'])}
if protected:
 new.update(protected);document['known']=dict(sorted(new.items()))
 path.write_text(json.dumps(document,indent=1)+'\n')
removed={k:v for k,v in old.items() if k not in new}
assert all(k.split('|')[2] in F and v['ref'] in ['TASK-214.15','TASK-214.16','TASK-214.17','TASK-214.18'] for k,v in removed.items()),'Out-of-scope known entry removed'
left={k:v for k,v in new.items() if k.split('|')[2] in F}
assert not [k for k,v in left.items() if v['ref'] in ['TASK-214.15','TASK-214.16','TASK-214.17','TASK-214.18']], 'Assigned strict entries remain'
summary={f:{'fixed':sum(k.split('|')[2]==f for k in removed),'left':sum(k.split('|')[2]==f for k in left)} for f in sorted(F)}
(D/'strict-entry-inventory.json').write_text(json.dumps(dict(removed=removed,left=left,per_family=summary,protected_baseline_listings=list(protected)),indent=2)+'\n')
print(len(removed),'fixed;',len(left),'out-of-scope entries retained')
print(json.dumps(summary,sort_keys=True))
