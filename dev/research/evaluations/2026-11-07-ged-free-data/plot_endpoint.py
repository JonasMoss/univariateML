#!/usr/bin/env python3
"""Render the rounded-endpoint counterexample; stdout is PNG base64.
Run under a CPU reservation or simbox-run. Uses existing mpmath and base R.
"""
import base64
import csv
from pathlib import Path
import math
import subprocess
import mpmath as mp
mp.mp.dps = 80
root = Path(__file__).resolve().parents[4]
image = root/'dev/research/evaluations/2026-11-07-ged-free/rounded-endpoint-path.png'
image.parent.mkdir(parents=True, exist_ok=True)
rounded = math.sqrt(3)
numerator,denominator = rounded.as_integer_ratio()
assert numerator*numerator < 3*denominator*denominator
obs = mp.mpf(rounded)
shape = [mp.power(10, mp.mpf(i)/4) for i in range(81)]
values = []
for nu in shape:
    logb = (mp.loggamma(1/nu)-mp.loggamma(3/nu))/2
    peak = -mp.log(2)-logb-mp.loggamma(1+1/nu)
    values.append(2*(peak-mp.exp(nu*(mp.log(obs)-logb))))
limit = -2*mp.log(2*mp.sqrt(3))
old = limit-2*mp.exp(-mp.euler)
assert values[-1] > old+1
csv_path=root/'build/scratch/endpoint-path.csv'
with csv_path.open('w') as out:
    writer=csv.writer(out)
    writer.writerow(['nu','loglik'])
    writer.writerows((str(float(x)),str(float(y))) for x,y in zip(shape,values))
result=subprocess.run(['Rscript',str(Path(__file__).with_suffix('.R')),str(csv_path),str(image),str(float(limit)),str(float(old)),str(1+math.exp(12))],text=True,capture_output=True)
if result.returncode:
    raise RuntimeError(result.stderr)
print(base64.b64encode(image.read_bytes()).decode())
