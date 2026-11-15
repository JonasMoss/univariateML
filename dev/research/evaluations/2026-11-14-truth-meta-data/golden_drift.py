"""Tables whose fitted sampler point no longer equals the golden estimate.

Reads only truth.json and golden.json (no generator, no CPU-heavy work).
"""
import json
import sys
from pathlib import Path

DISTS = Path(__file__).resolve().parents[4]/'univariateML-cpp/include/cppunivariate/distributions'
rows = []
for folder in sorted(DISTS.iterdir()):
    t, g = folder/'truth.json', folder/'golden.json'
    if not (t.exists() and g.exists()): continue
    table, golden = json.loads(t.read_text()), json.loads(g.read_text())
    row = table['sampler_points'][1][0]
    theta = table['parameters'][row]
    names = table['parameter_names']
    rel = max(abs(float(a)-float(golden['estimates'][n]))/max(abs(float(golden['estimates'][n])), 1e-300)
              for a, n in zip(theta, names))
    if rel > 0: rows.append((folder.name, rel))
rows.sort(key=lambda r: -r[1])
print(f'{len(rows)} tables differ from their golden fit; max relative difference {rows[0][1]:.3g}' if rows else 'none')
for name, rel in rows: print(f'{name}\t{rel:.3g}')
