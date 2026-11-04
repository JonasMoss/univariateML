import json,subprocess
from pathlib import Path
import numpy as np
HERE=Path(__file__).resolve().parent
ROOT=HERE.parents[3]
cases=json.loads((HERE/'inputs.json').read_text())
data=''.join(str(len(q['x']))+' '+' '.join(format(x,'.17g') for x in q['x'])+'\n' for q in cases)
rows=subprocess.check_output([str(ROOT/'build/scratch/t-cert-baseline')],input=data,text=True).splitlines()
results=[]
for i,case in enumerate(cases):
    row={'name':case['name']}
    for j,fam in enumerate(['lst','std']):
        vals=[float(v) for v in rows[2*i+j].split()]
        row[fam]={'status':int(vals[0]),'coefficients':[v if np.isfinite(v) else None for v in vals[1:4]],'logLik':vals[4] if np.isfinite(vals[4]) else None,'mean_seconds_3_repetitions':vals[5]}
    results.append(row)
(HERE/'baseline.json').write_text(json.dumps(results,indent=2)+'\n')
print('Recorded',len(results),'unchanged C++ fits for both families.')
