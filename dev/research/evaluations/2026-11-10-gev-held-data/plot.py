#!/usr/bin/env python3
"""One small report figure; requires an already installed matplotlib."""
import csv
import os
from pathlib import Path

root = Path(__file__).resolve().parent
repo = root.parents[3]
os.environ.setdefault('MPLCONFIGDIR', str(repo / 'build/tmp/matplotlib'))
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap

rows = list(csv.DictReader((root / 'study.csv').open()))
ns = [3, 10, 50, 100, 500]
scopes = [('gev', 'mu', 'GEV: location held', [-.9, -.5, 0, .5, 1, 2]),
          ('gev', 'sigma', 'GEV: scale held', [-.9, -.5, 0, .5, 1, 2]),
          ('frechet', 'sigma', 'Frechet: scale held', [.5, 1, 2])]
cmap = LinearSegmentedColormap.from_list('certificate', ['#fff1c4', '#a9d7c4', '#22785c'])
fig, axes = plt.subplots(1, 3, figsize=(10, 3.2), layout='constrained')
for ax, (family, hold, title, shapes) in zip(axes, scopes):
    values, annotations, stops = [], [], []
    for shape in shapes:
        vr, ar, sr = [], [], []
        for n in ns:
            cell = [r for r in rows if r['family'] == family and r['hold'] == hold
                    and float(r['shape']) == shape and int(r['n']) == n]
            count = len(cell)
            certified = sum(r['status'] == '0' and r['certified'] == '1' for r in cell)
            stopped = sum(r['status'] != '0' for r in cell)
            vr.append(certified / count)
            ar.append(f'{certified}/{count}')
            sr.append(stopped)
        values.append(vr); annotations.append(ar); stops.append(sr)
    im = ax.imshow(values, vmin=0, vmax=1, cmap=cmap, aspect='auto')
    for i, line in enumerate(annotations):
        for j, label in enumerate(line):
            ax.text(j, i, label, ha='center', va='center', fontsize=9,
                    color='#ad2424' if stops[i][j] else ('white' if values[i][j] > .8 else '#18372c'))
    ax.set_xticks(range(len(ns)), ns)
    ax.set_yticks(range(len(shapes)), [f'{s:g}' for s in shapes])
    ax.set_title(title, fontsize=10)
    ax.set_xlabel('Sample size')
    ax.set_ylabel('Generating xi' if family == 'gev' else 'Generating lambda')
    ax.tick_params(length=0)
fig.colorbar(im, ax=axes, label='Certified fraction', shrink=.75, ticks=[0, .5, 1])
fig.suptitle('Default budget: certified / total seeded fits', fontsize=12)
out = root.parent / '2026-11-10-gev-held' / 'certification.png'
out.parent.mkdir(exist_ok=True)
fig.savefig(out, dpi=150, facecolor='white')
plt.close(fig)
print(out)
