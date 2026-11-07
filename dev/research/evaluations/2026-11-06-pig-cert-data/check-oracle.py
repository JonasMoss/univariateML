"""Compare all C++ oracle rows against independent high precision results."""
import csv
import json
import math
from pathlib import Path
root = Path(__file__).parent
rows = list(csv.reader((root/'oracle-cpp.csv').open()))
truth = json.loads((root/'oracle.json').read_text())
assert len(rows) == len(truth) == 16
mean_errors, ll_errors = [], []
for i, (row, oracle) in enumerate(zip(rows, truth)):
    index, mean, ll, status = row
    assert int(index) == i and int(status) == 0, row
    mean_errors.append(abs(math.log(float(mean)/float(oracle['mean']))))
    ll_errors.append(abs(float(ll)-float(oracle['logLik'])))
assert max(mean_errors) < 1e-10, mean_errors
assert max(ll_errors) < 1e-10, ll_errors
print(json.dumps(dict(rows=len(rows), max_relative_mean_error=max(mean_errors), max_absolute_loglik_error=max(ll_errors)), indent=2))
