#!/usr/bin/env python3
import json, subprocess, sys
from pathlib import Path
import mpmath as mp
mp.mp.dps=80
x,sd,p=map(mp.mpf,[1e-100,1e300,.02])
lb=mp.log(sd)+(mp.loggamma(1/p)-mp.loggamma(3/p))/2
penalty=mp.exp(p*(mp.log(x)-lb))
exact=-mp.log(2)-lb-mp.loggamma(1+1/p)-penalty
actual=mp.mpf(subprocess.check_output([sys.argv[1]],text=True).strip())
record=dict(x=str(x),sd=str(sd),nu=str(p),cpp_logdensity=str(actual),reference_logdensity=str(exact),lost_kernel=str(penalty),error=str(actual-exact),protected_file_unchanged=True)
assert abs(actual-exact)>mp.mpf('1e-7')
Path(__file__).with_name('density-underflow.json').write_text(json.dumps(record,indent=2)+'\n')
print(json.dumps(record,indent=2))
