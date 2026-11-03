#!/usr/bin/env python3
"""Draw retained conditional profiles; does not rerun fits."""
import csv
import os
from pathlib import Path
here = Path(__file__).resolve().parent
os.environ.setdefault('MPLCONFIGDIR', str(here.parents[3]/'build/tmp/matplotlib'))
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

with (here/'profiles.csv').open() as handle:
    rows = list(csv.DictReader(handle))
fig, axes = plt.subplots(1, 2, figsize=(10.8, 4.1), layout='constrained')
styles = [('negative_slope_finite', 'Finite winner; Weibull locally stable', '#176ca4'),
          ('equal_faces_finite', 'Equal faces; finite winner', '#bc591d'),
          ('positive_slope_nonexistence', 'Pareto winner; Weibull slope positive', '#5a7852')]
for ax in axes:
    for name, label, color in styles:
        selected = [r for r in rows if r['sample'] == name]
        ax.semilogx([float(r['k']) for r in selected],
                    [float(r['gap_per_observation']) for r in selected],
                    label=label, color=color, linewidth=1.8)
    ax.axhline(0, color='#333333', linewidth=.7)
    ax.set_xlabel('Burr shape k (conditional numerical profiles)')
    ax.grid(alpha=.15)
axes[0].set_ylim(-.45, .025)
axes[0].set_ylabel('(profile log likelihood − better limit) / n')
axes[1].set_ylim(-.013, .013)
axes[1].set_title('Detail near the limit likelihood', fontsize=11)
axes[0].legend(fontsize=8, loc='lower left', frameon=False)
fig.savefig(here/'profiles.png', dpi=145)
print('Wrote profiles.png')
