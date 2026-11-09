"""Compare binary64 density probes with the independent 360-digit oracle."""
import csv
import json
import math
import pathlib
import sys

root = pathlib.Path(__file__).resolve().parent
reference = json.loads((root / 'std-overflow-oracle.json').read_text())
results = []
for row, target in zip(csv.reader(pathlib.Path(sys.argv[1]).open()), reference):
    assert float(row[1]) == target['x']
    for column, name in ((2, 'log_std'), (3, 'log_lst')):
        actual, expected = float(row[column]), float(target[name])
        error = abs(actual / expected - 1)
        assert math.isfinite(actual) and error < 1e-12
        results.append(dict(x=target['x'], quantity=name, actual=actual,
                            reference=expected, relative_error=error))
    assert float(row[4]) == 0
assert len(results) == 4
print(json.dumps(dict(checks=len(results),
                      max_relative_error=max(r['relative_error'] for r in results),
                      results=results), indent=2))
