"""Retain a reproducible diagnostic of the one refused shallow workload."""
import importlib
import json
from pathlib import Path
import roots
import sys

engine=importlib.import_module('global')
HERE=Path(__file__).resolve().parent
case=next(q for q in json.loads((roots.OLD/'inputs.json').read_text()) if q['name']=='normal-0')
rows=[]
if '--rerun' in sys.argv:
    for floor in [1,2]:
        result=engine.Engine(case['x'],floor).solve()
        rows.append({'floor':floor,**engine.strings(result)})
else:
    rows=[{'floor':1 if row['family']=='lst' else 2,**row}
          for row in json.loads((HERE/'global.json').read_text()) if row['name']=='normal-0']
(HERE/'shallow.json').write_text(json.dumps(rows,indent=2,allow_nan=False)+'\n')
print(json.dumps(rows,indent=2))
