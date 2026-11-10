"""Summarize the saved paired experiment without rerunning or retiming fits."""
import collections
import csv
import json
import statistics
from pathlib import Path

data = Path(__file__).resolve().parent
study = json.loads((data / 'seeded-study.json').read_text())
groups = collections.defaultdict(list)
for before, after in zip(study['rows']['legacy'], study['rows']['current']):
    assert before['index'] == after['index']
    assert before['sample_hash'] == after['sample_hash']
    key = tuple(after[k] for k in ['model', 'n', 'p', 'q', 'a'])
    groups[key].append((before, after))

fields = ['model', 'n', 'p', 'q', 'a', 'total', 'certified_finite',
          'certified_gamma', 'certified_power', 'uncertified_legacy',
          'legacy_mean_ms', 'current_mean_ms', 'current_median_ms', 'current_max_ms']
rows = []
for key, pairs in sorted(groups.items()):
    assert len(pairs) == 5
    row = dict(zip(fields[:5], key))
    counts = collections.Counter()
    for before, after in pairs:
        assert before['status'] == after['status']
        assert after['status'] in (0, 1, 2)
        category = ({0: 'certified_finite', 1: 'certified_gamma', 2: 'certified_power'}
                    [after['status']] if after['certified'] else 'uncertified_legacy')
        counts[category] += 1
    row.update(total=len(pairs), **{name: counts[name] for name in fields[6:10]})
    old_ms = [before['milliseconds'] for before, after in pairs]
    new_ms = [after['milliseconds'] for before, after in pairs]
    row.update(legacy_mean_ms=statistics.mean(old_ms), current_mean_ms=statistics.mean(new_ms),
               current_median_ms=statistics.median(new_ms), current_max_ms=max(new_ms))
    rows.append(row)

assert len(rows) == 44
assert sum(row['total'] for row in rows) == 220
assert [sum(row[k] for row in rows) for k in fields[6:10]] == [52, 52, 22, 94]
with (data / 'seeded-cell-summary.csv').open('w', newline='') as handle:
    writer = csv.DictWriter(handle, fieldnames=fields)
    writer.writeheader()
    writer.writerows(rows)
print('44 parameter/sample-size cells; all 220 paired fits accounted for')
