"""Replace the fitted sampler point of a table whose family cannot be fully regenerated.

For beta, betapr, nsbeta (3000 s timeout), pig, st (generator gap on an old
point) and sged (derivative mismatch on an old point), the whole-table
generator fails, but the table was only stale because its fitted sampler point
differs from the current golden estimate. This appends the freshly generated
fitted point (same generator, same checks) and points sampler_points[1] at it.
All existing points and rows stay byte-identical.

Usage (from dev/validation): PYTHONPATH=. python3 <this file> ID...
"""
import json
import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve().parents[4]/'dev/validation'))
from quality.truth import DISTS, make, read, input_sha256, mp, DIGITS

for ident in sys.argv[1:]:
    folder = DISTS/ident
    meta, g = read(folder)
    theta = {p['name']: float(g['estimates'][p['name']]) for p in meta['params']}
    path = folder/'truth.json'
    table = json.loads(path.read_text())
    with mp.workdps(DIGITS):
        new = make(folder, [theta])
    offset = len(table['parameters'])
    table['parameters'] += new['parameters']
    table['normalization'] += [dict(r, point=r['point']+offset) for r in new['normalization']]
    for key in ('values', 'quantiles'):
        table[key] += [[r[0]+offset, *r[1:]] for r in new[key]]
        if 'excluded_'+key in new:
            table.setdefault('excluded_'+key, []).extend(
                dict(r, row=[r['row'][0]+offset, *r['row'][1:]]) for r in new['excluded_'+key])
    table['sampler_points'][1] = [offset]
    table['input_sha256'] = input_sha256(folder)
    path.write_text(json.dumps(table, indent=2, allow_nan=False)+'\n')
    print(ident, 'fitted point appended as point', offset, flush=True)
