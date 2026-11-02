#!/usr/bin/env python3
import csv, math
from pathlib import Path
p=Path(__file__).resolve().parent
def read(name):
 with (p/name).open() as f:return list(csv.DictReader(f))
samples=read('samples.csv');cert=read('certified.csv');faces=read('faces.csv');cl=read('classification.csv');ext=read('extended.csv')
assert len(samples)==len(cert)==len(faces)==len(cl)==2202
assert all(int(c['index'])==i and int(f['index'])==i and int(k['index'])==i for i,(c,f,k) in enumerate(zip(cert,faces,cl)))
assert sum(int(c['failed']) for c in cert)==0
assert len(ext)==6
assert all(int(k['effective_Nmax'])==1000 for k in cl if int(k['index']) in {int(z['index']) for z in ext})
assert all(int(z['failed'])==0 for z in ext)
diffs=[abs(float(x['grid'])-float(y['loglik'])) for x,y in zip(samples,cert)]
assert max(diffs)<1e-6
idx=next(i for i,r in enumerate(samples) if r['data']=='0 1 2')
assert int(cert[idx]['N'])==2 and abs(float(cert[idx]['loglik'])+3*math.log(3))<1e-8
idx=next(i for i,r in enumerate(samples) if r['data']=='1 1 5') if any(r['data']=='1 1 5' for r in samples) else None
# The full h=3, M<=4 grid and the larger targeted profile grid are separate.
if idx is not None: assert float(faces[idx]['nb_slope'])<0 and float(cert[idx]['loglik'])>float(faces[idx]['nb'])
grid=read('profile-grid.csv'); row=next(z for z in grid if z['data']=='1-1-5' and z['N']=='5'); assert int(row['status'])==0 and abs(float(row['loglik'])+5.324190534666836)<1e-8
print('PASS: 2202 aligned samples, fixed-size certificates, extension, independent agreement and counterexamples')
print('maximum independent grid discrepancy',max(diffs))
