"""Collect provenance and check the retained research outcomes."""
from collections import Counter
from fractions import Fraction
import hashlib
import json
import platform
import subprocess
import sys
import numpy as np
import scipy
import mpmath
from isolate import HERE, PREV

r=json.loads((HERE/'global.json').read_text())
i=json.loads((HERE/'isolation.json').read_text())
v=json.loads((HERE/'verification.json').read_text())
e=json.loads((HERE/'exact-root.json').read_text())
b=json.loads((HERE/'benchmark.json').read_text())
counts=dict(Counter(a['outcome'] for a in r))
assert counts=={'global-finite':23,'global-Pareto':15,'global-Weibull':11,'subdivision-budget':1}
assert len(v['roots'])==i['roots']==33
assert i['maxima']==27
assert next(a for a in r if a['outcome']=='subdivision-budget')['sample']=='nearly-constant'
expected={'global-finite':0,'global-Weibull':1,'global-Pareto':2}
for a,old in zip(r,b):
    assert a['sample']==old['sample']
    if a['outcome'] in expected:assert old['existing_status']==expected[a['outcome']]
golden=next(a for a in i['certificates'] if a['sample']=='golden')
gglobal=next(a for a in r if a['sample']=='golden')
gbench=next(a for a in b if a['sample']=='golden')
metrics=dict(source_revision=subprocess.check_output(['git','rev-parse','HEAD'],text=True).strip(),
             environment=dict(python=sys.version,numpy=np.__version__,scipy=scipy.__version__,mpmath=mpmath.__version__,platform=platform.platform()),
             outcomes=counts,roots=i['roots'],maxima=i['maxima'],
             global_profiles=sum(a['profiles'] for a in r),global_seconds=sum(a['seconds'] for a in r),
             existing_seconds=sum(a['existing_seconds'] for a in b),
             median_existing_seconds=float(np.median([a['existing_seconds'] for a in b])),
             median_prototype_global_seconds=float(np.median([a['prototype_global_seconds'] for a in b])),
             golden_coefficient_bounds=[[float(Fraction(t)) for t in a] for a in e['root_box']],
             golden_relative_errors=[float(Fraction(a)) for a in e['coefficient_relative_errors_upper']],
             golden_exact_contraction_ratios=[float(Fraction(a)) for a in e['ratio_upper']],
             golden_root=golden['center'],golden_slab=golden['slab'],golden_global=gglobal,
             golden_existing_seconds=gbench['existing_seconds'],
             synthetic_seam_checks=len(v['synthetic_seam_checks']),
             nearzero_unsigned_finite_certificates=v['nearzero_unsigned_finite_certificates'],
             hashes={p.name:hashlib.sha256(p.read_bytes()).hexdigest() for p in sorted(HERE.iterdir()) if p.suffix in ['.py','.cpp','.sh','.md']},
             input_sha256=hashlib.sha256((PREV/'samples.json').read_bytes()).hexdigest())
(HERE/'summary.json').write_text(json.dumps(metrics,indent=2)+'\n')
print(json.dumps({k:a for k,a in metrics.items() if k not in ['hashes','environment','golden_global']},indent=2))
