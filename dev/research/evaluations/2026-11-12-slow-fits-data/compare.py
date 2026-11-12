"""Compare the raw core archive replay, excluding only elapsed time."""
import collections,json
from pathlib import Path
p=Path(__file__).resolve().parent
before=[json.loads(l) for l in (p/'before.jsonl').read_text().splitlines()]
after=[json.loads(l) for l in (p/'final.jsonl').read_text().splitlines()]
assert len(before)==len(after)==168
changes=[];success=collections.Counter()
for a,b in zip(before,after):
 assert (a['index'],a['family'])==(b['index'],b['family'])
 if a['result'].split()[:-1]!=b['result'].split()[:-1]:changes.append(dict(before=a,after=b))
 if a['result'].split()[0]=='0':
  assert a['result'].split()[:-1]==b['result'].split()[:-1]
  success[a['family']]+=1
result=dict(baseline='c4ed89f6a4222e133c7524cf726fea4cb9369593',rows=len(before),unchanged_successes=dict(success),changes=changes)
(p/'comparison.json').write_text(json.dumps(result,indent=2)+'\n');print(json.dumps(result,indent=2))
