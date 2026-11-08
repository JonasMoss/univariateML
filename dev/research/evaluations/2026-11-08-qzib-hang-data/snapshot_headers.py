#!/usr/bin/env python3
"""Read-only Git header snapshot for reproducing the baseline fit comparison.

Run from the repository root: snapshot_headers.py REVISION DESTINATION
The snapshot is an ordinary scratch directory, not another worktree.
"""
import argparse
from pathlib import Path
import subprocess

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('revision')
parser.add_argument('destination', type=Path)
args = parser.parse_args()
names = subprocess.check_output(['git', 'ls-tree', '-r', '--name-only', args.revision,
                                 'univariateML-cpp/include'], text=True).splitlines()
for name in names:
    if name.endswith('.hpp'):
        target = args.destination / name
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes(subprocess.check_output(['git', 'show', args.revision + ':' + name]))
