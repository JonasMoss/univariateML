#!/usr/bin/env python3
"""Run the TASK-214.3 defect cases against the baseline and the fixed headers.

Run from the repository root through /tmp/umvl-cpu/run:
    python3 dev/research/evaluations/2026-11-13-fix-nonfinite-data/before-after.py
The baseline headers are rebuilt from commit bbe1b13a with `git archive`
into build/scratch/base-bbe1b13a; the fixed headers are the working tree.
before-after.cpp uses only API common to both. Each build is native (long
double intermediates) and double-only (-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS),
GCC as installed, -std=gnu++17 -O2 -DNDEBUG. Output: before-after.txt.
"""
import subprocess, os
from pathlib import Path

ROOT = Path(__file__).resolve().parents[4]
HERE = Path(__file__).resolve().parent
BASE_COMMIT = 'bbe1b13a'
work = ROOT/'build/scratch'; work.mkdir(parents=True, exist_ok=True)
base = work/('base-'+BASE_COMMIT)
if not (base/'univariateML-cpp/include').exists():
    base.mkdir(parents=True, exist_ok=True)
    archive = subprocess.run(['git', 'archive', BASE_COMMIT, 'univariateML-cpp/include'], cwd=ROOT,
                             check=True, capture_output=True).stdout
    subprocess.run(['tar', '-x', '-C', str(base)], input=archive, check=True)
boost = os.environ.get('UMVL_BOOST_INCLUDE') or subprocess.check_output(
    ['Rscript', '-e', 'cat(system.file("include",package="BH"))'], text=True).strip()
lines = []
for tree, include in (('baseline '+BASE_COMMIT, base/'univariateML-cpp/include'), ('fix', ROOT/'univariateML-cpp/include')):
    for mode, flags in (('native', []), ('double', ['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'])):
        exe = work/('before-after-%s-%s' % (tree.split()[0], mode))
        subprocess.run(['c++', '-std=gnu++17', '-O2', '-DNDEBUG', *flags, '-I'+str(include), '-I'+boost,
                        str(HERE/'before-after.cpp'), '-o', str(exe)], check=True)
        lines.append('== %s, %s' % (tree, mode))
        lines.append(subprocess.check_output([str(exe)], text=True).rstrip())
(HERE/'before-after.txt').write_text('\n'.join(lines)+'\n')
print('\n'.join(lines))
