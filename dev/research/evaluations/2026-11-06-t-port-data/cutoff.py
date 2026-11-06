"""Independent 80-digit checks around both cancellation-series switches."""
import json,subprocess
from pathlib import Path
import mpmath as mp
mp.mp.dps=80
rows=[]
for line in subprocess.check_output(['build/scratch/t-port-cutoff'],text=True).splitlines():
    f=list(map(lambda s:mp.mpf(float(s)),line.split()));u=f[0]
    values=[u-mp.log1p(u),u+mp.expm1(-2*u)/2,-u+mp.expm1(2*u)/2]
    for j,value in enumerate(values):assert f[1+2*j]<=value<=f[2+2*j],(str(u),j,str(value),str(f[1+2*j]),str(f[2+2*j]))
    rows.append(dict(u=str(u),enclosed_quantities=3,passed=True))
Path(__file__).with_name('cutoff.json').write_text(json.dumps(rows,indent=2)+'\n')
print('Cancellation cutoff checks passed:',len(rows),'points x 3 quantities')
