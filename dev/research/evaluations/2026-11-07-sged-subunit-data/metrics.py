"""Summarize the paired deterministic benchmark; raw times are microseconds."""
import json
import statistics
from pathlib import Path
p = Path(__file__).resolve().parent
out = {}
for machine, suffix in [('workstation', ''), ('simbox', '-simbox')]:
    before = [[float(v) for v in row.split()] for row in (p/f'timings-before{suffix}.txt').read_text().splitlines()]
    after = [[float(v) for v in row.split()] for row in (p/f'timings-after{suffix}.txt').read_text().splitlines()]
    ratios = [a[-1]/b[-1] for a,b in zip(before,after)]
    oracle = json.loads((p/f'oracle{suffix}.json').read_text())['comparisons']
    out[machine] = dict(before_median_us=statistics.median(a[-1] for a in before),
        after_median_us=statistics.median(a[-1] for a in after),
        paired_median_speedup=statistics.median(ratios),
        minimum_speedup=min(ratios), maximum_speedup=max(ratios),
        maximum_relative_scale_error=max(a['relative_scale_error'] for a in oracle),
        maximum_loglik_error=max(a['loglik_error'] for a in oracle),
        maximum_conditional_scale_drift=max(abs(a[3]/b[3]-1) for a,b in zip(before,after)),
        maximum_conditional_loglik_change=max(abs(a[4]-b[4]) for a,b in zip(before,after)))
print(json.dumps(out,indent=2))
