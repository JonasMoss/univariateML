"""Replay every retained independent skew witness against the unmodified baseline."""
import json,math,sys,collections
from pathlib import Path
H=Path(__file__).resolve().parent;sys.path.insert(0,str(H));import audit
s=json.loads((H/'skew-smoke.json').read_text());audit.EXE=audit.WORK/'probe-baseline'
rows=[]
for r,actual in zip(s['rows'],audit.evaluate([r['job'] for r in s['rows']])):
 error=audit.error(actual,r['expected'],r['job'][1]==1 and not r['job'][4])
 if error>1e-10:rows.append(dict(job=r['job'],actual=actual,expected=r['expected'],error=error))
(H/'skew-baseline-failures.json').write_text(json.dumps(dict(checks=s['checks'],failures=rows),indent=2)+'\n')
print('independent skew baseline',s['checks'],'checks;',len(rows),'failures;',collections.Counter(r['job'][0] for r in rows))
