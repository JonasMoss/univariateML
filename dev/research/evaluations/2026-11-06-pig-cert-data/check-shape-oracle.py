import csv
import json
import math
from pathlib import Path
root=Path(__file__).parent
truth=json.loads((root/'shape-oracle.json').read_text())
rows=list(csv.reader((root/'shape-oracle-cpp.csv').open()))
assert len(rows)==len(truth)==10
mu_errors,shape_errors,ll_errors=[],[],[]
for i,(row,oracle) in enumerate(zip(rows,truth)):
    index,mu,shape,ll,status,iterations=row
    assert int(index)==i and int(status)==0,(row,oracle)
    mu_errors.append(abs(math.log(float(mu)/float(oracle['mean']))))
    shape_errors.append(abs(math.log(float(shape)/float(oracle['shape']))))
    ll_errors.append(abs(float(ll)-float(oracle['logLik'])))
assert max(mu_errors)<1e-9,mu_errors
assert max(shape_errors)<1e-6,shape_errors
assert max(ll_errors)<1e-8,ll_errors
print(json.dumps(dict(rows=len(rows),max_relative_mean_error=max(mu_errors),max_relative_shape_error=max(shape_errors),max_absolute_loglik_error=max(ll_errors)),indent=2))
