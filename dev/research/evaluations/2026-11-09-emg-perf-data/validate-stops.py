"""Compare final stop kinds/payload structure with the frozen baseline.
Run after reproduce.py (which creates the baseline overlay), under one slot.
"""
from pathlib import Path
import subprocess,csv,json,math
p=Path(__file__).resolve().parent;scratch=Path('build/scratch/emg-perf')
for version,include in [('before',scratch/'before-instrumented'),('after',Path('univariateML-cpp/include'))]:
    binary=scratch/f'stops-{version}'
    subprocess.run(['c++','-std=c++17','-O2',f'-I{include}','-IunivariateML-cpp/tests',str(p/'stop-check.cpp'),'-o',str(binary)],check=True)
    with (p/f'stops-{version}.csv').open('w') as out:subprocess.run([str(binary)],stdout=out,check=True)
a=list(csv.DictReader((p/'stops-before.csv').open()));b=list(csv.DictReader((p/'stops-after.csv').open()))
assert len(a)==len(b)==13
for old,new in zip(a,b):
    for key in ['case','status','budget','tie']:assert old[key]==new[key],(old,new)
    lower,upper=float(new['lower']),float(new['upper'])
    # Limit details carry a named face witness, not aggregate search bounds.
    assert math.isnan(lower)==math.isnan(upper),new
    if not math.isnan(lower):assert lower<=upper,new
    assert (int(old['candidates'])>0)==(int(new['candidates'])>0),(old,new)
(p/'stop-comparison.json').write_text(json.dumps(dict(cases=len(a),status_kinds_identical=True,payload_presence_identical=True),indent=2)+'\n')
print('13 baseline/final stop kinds and witness-presence checks passed')
