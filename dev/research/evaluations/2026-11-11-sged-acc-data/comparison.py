"""Join returned coefficients, independent roots and errors for review."""
import json
from pathlib import Path

D = Path(__file__).resolve().parent
roots = json.loads((D / 'all-certified-roots.json').read_text())['rows']
rows = []
for mode in ('native', 'double'):
    fits = {phase: {r['index']: r for r in map(json.loads,
            (D / f'{phase}-{mode}.jsonl').read_text().splitlines())}
            for phase in ('before', 'after')}
    refs = {phase: {r['index']: r for r in roots
                   if r['phase'] == phase and r['mode'] == mode}
            for phase in ('before', 'after')}
    for index, reference in refs['after'].items():
        row = dict(index=index, mode=mode, reference=reference['reference'])
        for phase in ('before', 'after'):
            fit, ref = fits[phase][index], refs[phase][index]
            row[phase] = dict(parameters=fit['parameters'], logLik=fit['logLik'],
                             component_errors=ref['components'],
                             maximum_scaled_error=ref['maximum_scaled_error'])
        rows.append(row)
(D / 'coefficient-comparison.json').write_text(json.dumps(rows, indent=2) + '\n')
print(f'Joined {len(rows)} comparisons across both arithmetic modes.')
