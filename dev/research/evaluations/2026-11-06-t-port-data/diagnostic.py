import json,subprocess
from pathlib import Path
here=Path(__file__).resolve().parent
cases=json.loads((here.parent/'2026-11-04-t-cert-data/inputs.json').read_text())
inputs=''.join(str(len(c['x']))+' '+' '.join(format(x,'.17g') for x in c['x'])+'\n' for c in cases)
result=subprocess.check_output(['build/scratch/t-port-diagnostic'],input=inputs,text=True)
(here/'diagnostic.txt').write_text(result)
for case,lines in zip(cases,zip(*[iter(result.splitlines())]*2)):print(case['name'],*lines,sep='\n')
