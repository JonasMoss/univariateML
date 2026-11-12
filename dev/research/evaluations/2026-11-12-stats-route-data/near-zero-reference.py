"""Independent 100-digit inverses at the exact binary64 inputs saved by R."""
import csv
from pathlib import Path
import mpmath as mp
mp.mp.dps = 100
root = Path(__file__).resolve().parent
rows = []
with (root / 'ordinary-changes.csv').open() as stream:
    for row in csv.DictReader(stream):
        if row['id'] not in ('norm', 'logis'):
            continue
        theta = {k: mp.mpf(float(v)) for k, v in
                 (term.split('=') for term in row['parameters'].split(';'))}
        probability = mp.mpf(float.fromhex(row['input_hex']))
        if row['logged'] == 'TRUE':
            probability = mp.exp(probability)
        if row['lower'] == 'FALSE':
            probability = 1 - probability
        if row['id'] == 'norm':
            reference = theta['mean'] + theta['sd'] * mp.sqrt(2) * mp.erfinv(2*probability-1)
        else:
            reference = theta['location'] + theta['scale'] * mp.log(probability/(1-probability))
        core = mp.mpf(float.fromhex(row['core_hex']))
        stats = mp.mpf(float.fromhex(row['stats_hex']))
        rows.append(dict(row, reference=mp.nstr(reference, 30),
                         core_relative_error=mp.nstr(abs((core-reference)/reference), 12),
                         stats_relative_error=mp.nstr(abs((stats-reference)/reference), 12)))
with (root / 'near-zero-reference.csv').open('w') as stream:
    writer = csv.DictWriter(stream, fieldnames=list(rows[0]))
    writer.writeheader()
    writer.writerows(rows)
print('mpmath', mp.__version__, 'precision', mp.mp.dps, 'rows', len(rows))
print('maximum relative error: core', max(float(r['core_relative_error']) for r in rows),
      'stats', max(float(r['stats_relative_error']) for r in rows))
