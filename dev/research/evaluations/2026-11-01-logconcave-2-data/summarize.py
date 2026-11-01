import collections
import csv
import math
from pathlib import Path

base = Path(__file__).resolve().parent
for family in ('sn', 'ged', 'sged'):
    with (base / (family + '-inner.csv')).open() as stream:
        rows = list(csv.DictReader(stream))
    statuses = collections.Counter(int(row['status']) for row in rows)
    pairs = [(float(row['old_ll']), float(row['inner_ll'])) for row in rows]
    differences = [b-a for a, b in pairs if math.isfinite(a) and math.isfinite(b)]
    print(family, 'rows', len(rows), 'status', dict(sorted(statuses.items())),
          'finite comparisons', len(differences), 'min delta', min(differences, default=math.nan),
          'max delta', max(differences, default=math.nan),
          'loss > 1e-12 relative', sum(d < -1e-12*max(1,abs(a)) for (a,b) in pairs if math.isfinite(a) and math.isfinite(b) for d in [b-a]))
