"""Recheck every originally failing scalar seam row, without replacing its reference."""
import json,sys,math,collections
from pathlib import Path
H=Path(__file__).resolve().parent;sys.path.insert(0,str(H));import audit
s=json.loads((H/'seam-row-failures.json').read_text());rows=s['failures']
jobs=[(r['family'],0,r['input'],True,True,r['parameters']) for r in rows]
fails=[];named=[];errors=[]
for r,actual in zip(rows,audit.evaluate(jobs)):
 e=audit.error(actual,r['expected']);errors.append(e)
 if math.isnan(actual):named.append(dict(r,current=actual,current_error=e))
 elif e>1e-10:fails.append(dict(r,current=actual,current_error=e))
(H/'seam-row-replay.json').write_text(json.dumps(dict(checks=len(rows),finite_failures=fails,named_precision_regions=named,worst=max([e for e in errors if math.isfinite(e)]+[0])),indent=2)+'\n')
print('original scalar seam replay',len(rows),'checks;',len(fails),'finite failures;',len(named),'named NaNs')
assert not fails
