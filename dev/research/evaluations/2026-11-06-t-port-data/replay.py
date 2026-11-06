"""Frozen 31-sample replay and native timing comparison, no fixture writes."""
import json, subprocess
from pathlib import Path
here=Path(__file__).resolve().parent
root=here.parents[3]
cases=json.loads((here.parent/'2026-11-04-t-cert-data/inputs.json').read_text())
inputs=''.join(str(len(c['x']))+' '+' '.join(format(x,'.17g') for x in c['x'])+'\n' for c in cases)
labels=['finite','cauchy','normal','t2','collapse','unbounded','budget','tie','numerical_failure','invalid_data']
p=subprocess.Popen([str(root/'build/scratch/t-port-replay')],stdin=subprocess.PIPE,stdout=subprocess.PIPE,text=True)
p.stdin.write(inputs);p.stdin.close()
rows=[]
def number(s):
    v=float(s)
    return v if abs(v)<float('inf') else s
for case in cases:
    for family in ['lst','std']:
        line=p.stdout.readline();assert line,(case['name'],family)
        f=line.split();status=int(f[1]);row=dict(name=case['name'],family=family,outcome=labels[status],mu=number(f[2]),ordinary_scale=number(f[3]),nu=number(f[4]),logLik=number(f[5]),profiles=int(f[6]),seconds=float(f[7]),coefficient_box=[[number(f[8+2*j]),number(f[9+2*j])] for j in range(3)],baseline=dict(status=int(f[14]),seconds_3_repetitions=float(f[15]),mu=number(f[16]),scale=number(f[17]),nu=number(f[18]),logLik=number(f[19])))
        rows.append(row)
        print(case['name'],family,row['outcome'],row['profiles'],row['seconds'],flush=True)
        (here/'replay.json').write_text(json.dumps(rows,indent=2,allow_nan=False)+'\n')
assert p.wait()==0
print('Completed native replay:',len(rows))
