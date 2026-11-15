"""Retain the brief's expressly excluded entries if a judge prune removes one."""
import json
from pathlib import Path
here=Path(__file__).resolve().parent
path=here.parents[3]/'dev/validation/strict-known.json'
original=json.loads((here/'known-before.json').read_text());current=json.loads(path.read_text())
restored=[]
for key,value in original.items():
 if value['ref'] in ['TASK-214.5','TASK-214.13'] and key not in current['known']:
  current['known'][key]=value;restored.append(key)
if restored:
 current['known']=dict(sorted(current['known'].items()));path.write_text(json.dumps(current,indent=1)+'\n')
print(json.dumps(dict(retained_excluded_entries=restored),indent=2))
