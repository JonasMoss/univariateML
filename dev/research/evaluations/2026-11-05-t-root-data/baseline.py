"""Contemporaneous unmodified native fitting times on the frozen inputs."""
import json
import subprocess
from pathlib import Path
import roots

HERE=Path(__file__).resolve().parent
ROOT=HERE.parents[3]
cases=json.loads((roots.OLD/'inputs.json').read_text())
data=''.join(str(len(q['x']))+' '+' '.join(format(x,'.17g') for x in q['x'])+'\n' for q in cases)
lines=subprocess.check_output([str(ROOT/'build/scratch/t-root-baseline')],input=data,text=True).splitlines()
rows=[]
for i,case in enumerate(cases):
    row={'name':case['name']}
    for j,family in enumerate(['lst','std']):
        fields=lines[2*i+j].split()
        row[family]={'status':int(fields[0]),'mean_seconds_3_repetitions':float(fields[5])}
    rows.append(row)
(HERE/'baseline.json').write_text(json.dumps(rows,indent=2)+'\n')
print('Native timing checks completed:',len(rows),'samples x 2 families')
