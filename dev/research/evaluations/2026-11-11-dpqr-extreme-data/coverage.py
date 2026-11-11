"""Inventory attempted extreme paths and dossier limit statements, without claiming a proof."""
import json,re,sys
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4];HERE=Path(__file__).resolve().parent
sys.path.insert(0,str(ROOT/'dev/validation'))
from quality.truth import DISTS,read,module_at,extreme_points,encode
records=[]
for folder in sorted(DISTS.iterdir()):
 if not (folder/'truth.json').exists():continue
 meta,_=read(folder);route=module_at(folder/'truth.py')
 measured=HERE/'measured-tables'/(folder.name+'.json')
 table=json.loads((measured if measured.exists() else folder/'truth.json').read_text())
 planned=extreme_points(meta,route)
 records.append(dict(family=folder.name,parameter_names=[p['name'] for p in meta['params']],
  requested=[[encode(t[p['name']]) for p in meta['params']] for t in planned],
  attained=table['parameters'],excluded_parameter_points=[dict(parameters=r['parameters'],reason=r['reason']) for r in table.get('excluded_parameter_points',[])],oracle_gaps=table.get('extreme_reference_gaps',[]),
  joint_paths=[{key:encode(value) for key,value in path.items()} for path in getattr(route,'extreme_parameter_sets',[])],
  dossier_limit_statements=[dict(line=i,text=line) for i,line in enumerate((folder/'README.md').read_text().splitlines(),1)
    if re.search(r'escape|limit model|converge|infinity|→|\\to|\\infty',line,re.I)]))
(HERE/'coverage.json').write_text(json.dumps(records,indent=2,allow_nan=False)+'\n')
print('coverage inventory',len(records),'families')
