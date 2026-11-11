#!/usr/bin/env python3
"""Run under run4; temporary adapter static_assert edits are always restored."""
import os
from pathlib import Path
import subprocess
import time
import json

root = Path.cwd()
output = root / 'dev/research/evaluations/2026-11-11-test-scope-data'
adapter = root / 'bindings/r/distributions/kumar/adapter.cpp'
# Rebuild the recorded numerical baseline, not an archived header tree.
baseline = '200b501b'
original = subprocess.check_output(['git', 'show', baseline + ':bindings/r/distributions/kumar/adapter.cpp'])
if adapter.read_bytes() != original:
    raise SystemExit('Restore the baseline adapter before reproducing the study')
subprocess.run(['git', 'diff', '--exit-code', baseline, '--', 'univariateML-cpp',
                'bindings/common', 'bindings/r/distributions'], check=True)
env = os.environ.copy()
env['UMVL_MEASURE_SKIP_TESTS'] = '1'
results = []


def run(label, command, groups=4, compare=False):
    current = env.copy()
    current['UMVL_UNITY_GROUPS'] = str(groups)
    if compare:
        current['UMVL_REPRO_COMPARE'] = str(root / 'univariateML-r')
    start = time.monotonic()
    with (root / f'build/scratch/{label}.log').open('w') as log:
        subprocess.run(command, env=current, stdout=log, stderr=subprocess.STDOUT, check=True)
    row = dict(label=label, command=command, groups=groups, seconds=round(time.monotonic()-start, 2))
    results.append(row)
    (output / 'r-study.json').write_text(json.dumps(results, indent=2)+'\n')
    print(row, flush=True)


def edit(tag):
    adapter.write_bytes(original + f'\nstatic_assert(true, "TASK-193 {tag} rebuild measurement");\n'.encode())


try:
    run('r-warm-wipe', ['bash', 'dev/research/evaluations/2026-11-11-test-scope-data/measure-r-rebuild.sh', 'warm-wipe'])
    edit('wipe-unity')
    run('r-edit-wipe', ['bash', 'dev/research/evaluations/2026-11-11-test-scope-data/measure-r-rebuild.sh', 'edit-wipe'])
    adapter.write_bytes(original)
    run('r-reset-unity', ['bash', 'dev/research/evaluations/2026-11-11-test-scope-data/experimental-r-incremental-regen.sh'])
    edit('incremental-unity')
    run('r-edit-incremental-unity', ['bash', 'dev/research/evaluations/2026-11-11-test-scope-data/experimental-r-incremental-regen.sh'])
    run('r-identity-unity', ['bash', 'dev/tools/r-assembly-reproducibility.sh'], compare=True)
    adapter.write_bytes(original)
    run('r-per-family-first', ['bash', 'dev/research/evaluations/2026-11-11-test-scope-data/measure-r-rebuild.sh', 'per-family-first'], groups=0)
    edit('wipe-family')
    run('r-edit-wipe-family', ['bash', 'dev/research/evaluations/2026-11-11-test-scope-data/measure-r-rebuild.sh', 'edit-wipe-family'], groups=0)
    adapter.write_bytes(original)
    run('r-reset-family', ['bash', 'dev/research/evaluations/2026-11-11-test-scope-data/experimental-r-incremental-regen.sh'], groups=0)
    edit('incremental-family')
    run('r-edit-incremental-family', ['bash', 'dev/research/evaluations/2026-11-11-test-scope-data/experimental-r-incremental-regen.sh'], groups=0)
    run('r-identity-family', ['bash', 'dev/tools/r-assembly-reproducibility.sh'], groups=0, compare=True)
finally:
    adapter.write_bytes(original)
