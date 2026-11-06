"""Native exhaustive decisions on the retained multimodal counterexamples."""
import json,subprocess
from pathlib import Path
here=Path(__file__).resolve().parent
cases=json.loads((here.parent/'2026-11-04-t-closure-data/multimodality.json').read_text())
rows=[]
for name,case in cases.items():
    x=[float(v) for v,w in zip(case['x'],case['weights']) for _ in range(int(w))]
    inputs=str(len(x))+' '+' '.join(format(v,'.17g') for v in x)+'\n'
    lines=subprocess.check_output(['build/scratch/t-port-replay'],input=inputs,text=True).splitlines()
    winner=max(case['stationary_candidates'],key=lambda p:float(p['ll']))
    for family,line in zip(['lst','std'],lines):
        f=line.split();status=int(f[1]);row=dict(name=name,family=family,status=status,profiles=int(f[6]),seconds=float(f[7]))
        if status==0:
            for j,key in enumerate(['mu','scale','nu']):
                lo,hi=float(f[8+2*j]),float(f[9+2*j]);assert lo<float(winner[key])<hi,(name,family,key,lo,winner[key],hi)
            row['winner_enclosed']=True
        else:
            assert status in (6,7,8),(name,family,status)
            row['guarded_refusal']=True
        rows.append(row)
        print(name,family,row,flush=True)
(here/'adversarial.json').write_text(json.dumps(rows,indent=2)+'\n')
