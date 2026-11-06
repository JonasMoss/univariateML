#!/usr/bin/env python3
"""Record foreground final checks. Invoke through /tmp/umvl-cpu/run4.

  /tmp/umvl-cpu/run4 python3 dev/research/evaluations/2026-11-06-burr-scale-data/run_checks.py

Every child inherits the CPU reservation, nice priority and one BLAS thread.
No child takes another slot lock. Logs stay in ignored build/scratch/.
"""
import json
import hashlib
import platform
import os
from pathlib import Path
import subprocess
import sys
import time
root = Path(__file__).resolve().parents[4]
os.chdir(root)
folder = Path(__file__).resolve().parent
sources = []
for family in ['burr', 'invburr', 'pareto4', 'gb2', 'fpareto']:
    directory = root / 'univariateML-cpp/include/cppunivariate/distributions' / family
    for name in ['README.md', 'mle.hpp', 'test.cpp', 'golden.json', 'fixed-scale-golden.json', 'scale_certificate.hpp']:
        path = directory / name
        if path.exists():
            sources.append(path)
    sources.append(root / 'bindings/r/distributions' / family / 'test.R')
manifest = {'base_revision': subprocess.check_output(['git', 'rev-parse', 'HEAD'], text=True).strip(),
            'machine': platform.platform(),
            'compiler': subprocess.check_output(['c++', '--version'], text=True).splitlines()[0],
            'source_sha256': {str(p.relative_to(root)): hashlib.sha256(p.read_bytes()).hexdigest() for p in sources}}
(folder / 'source-manifest.json').write_text(json.dumps(manifest, indent=2)+'\n')
commands = [('cpp-dev', ['just', 'cpp-test'], {}),
            ('cpp-no-long-double', ['just', 'cpp-test-no-long-double'], {}),
            ('r-dev', ['just', 'test'], {}),
            ('js-build', ['just', 'js-build'], {}),
            ('js-test', ['just', 'js-test'], {}),
            ('site-check', ['just', 'site-check'], {}),
            ('cpp-full', ['just', 'cpp-test'], {'UMVL_TEST_TIER': 'full'}),
            ('r-full', ['just', 'test-full'], {})]
if len(sys.argv) > 1:
    selected = set(sys.argv[1:])
    commands = [c for c in commands if c[0] in selected]
for label, command, overrides in commands:
    env = dict(os.environ)
    env.update(overrides)
    log = root / 'build' / 'scratch' / (label + '-final.log')
    print('Starting', label, flush=True)
    start = time.monotonic()
    with log.open('w') as output:
        result = subprocess.run(command, env=env, stdout=output, stderr=subprocess.STDOUT)
    record = dict(label=label, command=command, environment=overrides,
                  machine='workstation', seconds=time.monotonic()-start,
                  exit_code=result.returncode, log=str(log.relative_to(root)))
    with (folder / 'runs.jsonl').open('a') as output:
        output.write(json.dumps(record)+'\n')
    print(json.dumps(record), flush=True)
    if result.returncode:
        sys.exit(result.returncode)
