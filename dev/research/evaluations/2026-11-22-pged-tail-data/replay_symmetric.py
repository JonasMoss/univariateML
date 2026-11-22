"""Replay the TASK-220 symmetric SGED cases through their pged delegate."""
import gzip
import json
import math
import subprocess
import sys

import mpmath as mp

from sweep import BIN, DATA, error

source = DATA.parent / '2026-11-14-sged-tail-data' / 'reference.json.gz'
with gzip.open(source, 'rt') as stream:
    rows = json.load(stream)['rows']

inputs = []
references = []
for row in rows:
    mean, sd, nu, xi = row['theta']
    if row['family'] != 'sged' or xi != 1:
        continue
    for tail, value in enumerate(row['ref']):
        lp = mp.mpf(value)
        if not mp.isfinite(lp) or lp == 0:
            continue
        for log in (False, True):
            ref = lp if log else mp.exp(lp)
            if not log and ref < sys.float_info.min:
                continue
            inputs.append(f"p {row['x']!r} {mean!r} {sd!r} {nu!r} {int(tail == 0)} {int(log)}\n")
            references.append((ref, not log))

results = {}
for mode in ('native', 'double'):
    values = subprocess.check_output([BIN / f'gated-{mode}'], input=''.join(inputs), text=True).splitlines()
    errors = [error(float(value), ref, relative)
              for value, (ref, relative) in zip(values, references, strict=True)]
    results[mode] = dict(checks=len(errors), worst=max(errors), failures=sum(e > 1e-10 for e in errors))
    print(mode, results[mode])
    assert results[mode]['failures'] == 0
(DATA / 'symmetric-sged.json').write_text(json.dumps(results, indent=2) + '\n')
