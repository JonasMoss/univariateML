"""Measure CDF CPU time with parsing excluded, median of three runs."""
import json
import statistics
import subprocess

from sweep import BIN, DATA

grouped = {}
for row in (BIN / 'inputs').read_text().splitlines(keepends=True):
    op, x, mean, sd, nu, lower, log = row.split()
    if op != 'p' or (mean, sd, lower, log) != ('0', '1', '0', '0') or float(x) <= 0:
        continue
    grouped.setdefault(nu, []).append(row)
# Omit subnormals and the exact underflow region in the repeated timing.
for nu, rows in grouped.items():
    grouped[nu] = rows[:12]
warm = [row for rows in grouped.values() for _ in range(64) for row in rows]
cold = [rows[9] for _ in range(4) for rows in grouped.values()]
# Keep the ordinary benchmark grouped too, so the only difference is shape.
ordinary = [row for nu, rows in grouped.items() if float(nu) <= 10
            for _ in range(256) for row in rows[:8]]
workloads = {'ordinary': ordinary, 'grouped': warm, 'changing': cold}
outputs = {}
for binary in sorted(BIN.glob('*-*')):
    if not binary.is_file():
        continue
    measurements = {}
    for name, rows in workloads.items():
        inp = ''.join(rows)
        times = []
        for _ in range(3):
            elapsed, checksum = subprocess.check_output([binary, '--time'], input=inp, text=True).split()
            assert float(checksum) > 0, (binary, name, checksum)
            times.append(float(elapsed))
        measurements[name] = {'calls': len(rows), 'seconds': times,
                              'us_per_call': statistics.median(times) * 1e6 / len(rows)}
    outputs[binary.name] = measurements
    print(binary.name, {k: round(v['us_per_call'], 3) for k, v in measurements.items()}, flush=True)
(DATA / 'timing.json').write_text(json.dumps(outputs, indent=2) + '\n')
