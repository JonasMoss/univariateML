#!/usr/bin/env python3
"""Source-loop inventory and byte comparison of frozen fixtures against HEAD.

Run from the repository root through the CPU slot wrapper. No files are modified
unless --output is supplied; this is evidence inventory, not an accuracy oracle.
"""
import argparse
import hashlib
import json
from pathlib import Path
import re
import subprocess

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--output', type=Path)
args = parser.parse_args()
root = Path('univariateML-cpp/include/cppunivariate')
files = sorted((root / 'distributions').glob('*/dpqr*.hpp'))
files += sorted((root / 'special').glob('*.hpp'))
inventory = []
for path in files:
    text = path.read_text()
    inventory.append({'path': str(path), 'sha256': hashlib.sha256(path.read_bytes()).hexdigest(),
                      'loops': [{'line': i, 'text': line.strip()}
                                for i, line in enumerate(text.splitlines(), 1)
                                if re.search(r'\b(?:while|for)\s*\(', line)]})
tracked = subprocess.check_output(['git', 'ls-files', str(root / 'distributions')], text=True).splitlines()
fixtures = [p for p in tracked if Path(p).name in ('golden.json', 'truth.json', 'truth.py')]
changed = []
combined = hashlib.sha256()
for name in fixtures:
    current = Path(name).read_bytes()
    baseline = subprocess.check_output(['git', 'show', 'HEAD:' + name])
    if current != baseline:
        changed.append(name)
    combined.update(name.encode() + b'\0' + current + b'\0')
result = {'head': subprocess.check_output(['git', 'rev-parse', 'HEAD'], text=True).strip(),
          'dpqr_families': len(list((root / 'distributions').glob('*/dpqr.hpp'))),
          'fixture_files': len(fixtures), 'fixture_changes': changed,
          'fixture_aggregate_sha256': combined.hexdigest(), 'source_inventory': inventory,
          'changed_tracked_sha256': {
              name: hashlib.sha256(Path(name).read_bytes()).hexdigest()
              for name in subprocess.check_output(['git', 'diff', '--name-only', 'HEAD'], text=True).splitlines()
              if Path(name).is_file()}}

output = json.dumps(result, indent=2) + '\n'
if args.output:
    args.output.write_text(output)
else:
    print(output, end='')
if changed:
    raise SystemExit(1)
