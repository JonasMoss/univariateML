"""Record the 201-count test windows, including binary64 overflow cases."""
import json,math
from pathlib import Path
out=Path(__file__).parent
root=Path('univariateML-cpp/include/cppunivariate/distributions')
rows=[]
for id in json.loads((out/'families.json').read_text()):
 t=json.loads((root/id/'truth.json').read_text())
 for point,theta in enumerate(t['parameters']):
  medians=[float(q[4]) for q in t['quantiles'] if q[:4]==[point,1,0,.5]]
  median=medians[0] if medians else float('nan')
  reason='oracle median'
  if not math.isfinite(median): centre=0;reason='bulk outside finite binary64 range'
  elif abs(median)>9007199254740890:centre=0;reason='unit count window outside exact integer range'
  else:centre=math.floor(median)
  rows.append(dict(family=id,point=point,parameters=theta,oracle_median=str(median),centre=centre,first=centre-100,last=centre+100,reason=reason))
(out/'windows.json').write_text(json.dumps(rows,indent=2)+'\n')
for row in rows:
 if row['reason']!='oracle median': print(row)
