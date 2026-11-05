#!/usr/bin/env python3
"""Numerical illustration of the exact nonuniqueness construction in README.
The proof uses a global maximizer of a scalar function; this check locates
one candidate numerically and verifies the production fit's near-tie stop.
Run through /tmp/umvl-cpu/run from the worktree root.
"""
import importlib.util
import json
import math
from pathlib import Path
import subprocess
import sys
sys.dont_write_bytecode=True
import mpmath as mp

HERE=Path(__file__).resolve().parent
spec=importlib.util.spec_from_file_location('certificate_check',HERE/'check_certificate.py')
check=importlib.util.module_from_spec(spec);spec.loader.exec_module(check)
mp.mp.dps=80

def a_of(k):
    b=k*mp.exp(1+k)
    return mp.expm1((3*k-mp.log1p(b))/2)/b

k=check.refine(lambda z:mp.diff(a_of,z),mp.mpf('.6'))
a=a_of(k)
assert mp.diff(a_of,k,2)<0
check.prepare()
rows=[]
for label,delta in [('interior',-1e-5),('tie',0.),('uniform',1e-5)]:
    value=float(a)+delta
    payload=f'3 1 nan nan {value!r} {value!r} 1.0\n'
    run=subprocess.run([str(check.SCRATCH/'runner')],input=payload,text=True,capture_output=True,check=True)
    fields=list(map(float,run.stdout.split()));status,reason=map(int,fields[:2])
    if label=='tie':assert status==3 and reason==3,run.stdout
    else:assert status==0,run.stdout
    if label=='uniform':assert fields[3]==-1,run.stdout
    if label=='interior':assert fields[3]>-1 and fields[4]>0,run.stdout
    rows.append(dict(label=label,a=value,status=status,certificate_failure=reason,
                     sigma=fields[2] if math.isfinite(fields[2]) else None,
                     xi=fields[3] if math.isfinite(fields[3]) else None,
                     loglik=fields[4] if math.isfinite(fields[4]) else None,
                     work=int(fields[5])))
result=dict(critical_a_numeric=mp.nstr(a,75),critical_xi_numeric=mp.nstr(k,75),
            critical_sigma_numeric=mp.nstr(mp.exp(-1-k),75),
            note='Scalar local root numerical; exact global-max construction and proof are in gpd/README.md.',
            cases=rows)
(HERE/'uniqueness-validation.json').write_text(json.dumps(result,indent=2,allow_nan=False)+'\n')
print(json.dumps(result,indent=2))
