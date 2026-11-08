"""Interleave precompiled baseline/current binaries; report seven-run medians."""
import json
import os
import statistics
import subprocess
if hasattr(os, 'sched_getaffinity'):
    os.sched_setaffinity(0, {min(os.sched_getaffinity(0))})
observations = {name: {i: [] for i in range(3)} for name in ('before','after')}
for trial in range(7):
    order = ('before','after') if trial % 2 == 0 else ('after','before')
    for name in order:
        output = subprocess.check_output([f'build/scratch/bench-{name}-final'], text=True)
        for line in output.splitlines():
            case, status, alpha, beta, elapsed = line.split()
            case = int(case)
            assert int(status) == (0 if case == 0 else 3)
            observations[name][case].append(float(elapsed))
print(json.dumps({'fits_per_run':100, 'trials':7,
                  'cpu_affinity':sorted(os.sched_getaffinity(0)),
                  'seconds_per_100_fits':observations,
                  'median_seconds_per_100_fits':{
                      name:{case:statistics.median(values) for case,values in groups.items()}
                      for name,groups in observations.items()}},indent=2))
