#!/usr/bin/env python3
"""Run disjoint Node registration groups and merge their truth diagnostics."""
import json
import os
from pathlib import Path
import subprocess
import sys

root = Path(__file__).resolve().parents[2]
os.chdir(root)
scratch = root / 'build/scratch'
scratch.mkdir(parents=True, exist_ok=True)
for old in scratch.glob('truth-results-*.json'):
    old.unlink()
setting = os.getenv('CMAKE_BUILD_PARALLEL_LEVEL') or '1'
try:
    workers = int(setting)
    if workers < 1:
        raise ValueError()
except ValueError:
    sys.exit('CMAKE_BUILD_PARALLEL_LEVEL must be a positive integer')
entries = sorted((root / 'bindings/js/test-groups').glob('*.mjs'))
if not entries:
    sys.exit('No JS registration groups found')
# Use one module instance in serial mode. Sixteen smaller groups were faster
# than four in the measured four-worker runs.
group_count = 1 if workers == 1 else len(entries)
env = os.environ.copy()
env['UMVL_JS_GROUP_COUNT'] = str(group_count)
command = ['node', '--v8-pool-size=1', '--test', '--test-concurrency=' + str(workers)]
command += [str(p.relative_to(root)) for p in entries[:group_count]]
code = subprocess.call(command, env=env)
records = [json.loads(p.read_text()) for p in sorted(scratch.glob('truth-results-*.json'))]
(scratch / 'truth-results.json').write_text(json.dumps({
    'checks': sum(r['checks'] for r in records),
    'failures': [f for r in records for f in r['failures']],
}, indent=2) + '\n')
if len(records) != group_count:
    print(f'Missing truth diagnostics: {len(records)}/{group_count} groups', file=sys.stderr)
    code = code or 1
sys.exit(code)
