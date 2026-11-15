"""Inventory the assigned ratchet entries, without adding or changing any."""
import collections,json,subprocess
from pathlib import Path
here=Path(__file__).resolve().parent;root=here.parents[3]
before=json.loads(subprocess.check_output(['git','show','HEAD:dev/validation/strict-known.json'],cwd=root))['known']
after=json.loads((root/'dev/validation/strict-known.json').read_text())['known']
assigned=json.loads((here/'known-before.json').read_text())
assert not set(after)-set(before),'new ratchet entries are forbidden'
removed={k:v for k,v in assigned.items() if k not in after}
retained={k:v for k,v in assigned.items() if k in after}
assert not [k for k,v in retained.items() if v['ref'] in ['TASK-214.15','TASK-214.16','TASK-214.17']],'targeted entries remain'
assert not [k for k,v in removed.items() if v['ref'] in ['TASK-214.5','TASK-214.13']],'excluded entries removed'
assert not (set(before)-set(after))-set(assigned),'another job family was pruned'
records=dict(removed=removed,retained=retained,removed_by_ref=dict(collections.Counter(v['ref'] for v in removed.values())),retained_by_ref=dict(collections.Counter(v['ref'] for v in retained.values())),removed_by_family=dict(collections.Counter(k.split('|')[2] for k in removed)),retained_by_family=dict(collections.Counter(k.split('|')[2] for k in retained)))
(here/'known-diff.json').write_text(json.dumps(records,indent=2)+'\n')
print(json.dumps({k:v for k,v in records.items() if k not in ['removed','retained']},indent=2))
