#!/usr/bin/env python3
"""Collect completed benchmark evidence; run through /tmp/umvl-cpu/run."""
from collections import Counter
import gzip
import hashlib
import json
from pathlib import Path
import re

root = Path(__file__).resolve().parents[4]
data = Path(__file__).resolve().parent
scratch = root / 'build/scratch'


def measurement(name):
    text = (scratch / (name + '.log')).read_text()
    def number(pattern):
        return float(re.findall(pattern, text)[-1])
    results = re.findall(r'(\d+)% tests passed, (\d+) tests failed out of (\d+)', text)
    percent, failed, total = map(int, results[-1])
    cache_file = scratch / (name + '-ccache.log')
    cache = Counter(re.findall(r'\bResult: (\w+)', cache_file.read_text())) if cache_file.exists() else Counter()
    return {
        'machine': 'workstation', 'workers': 4,
        'build_seconds': number(r'build_wall_seconds=([\d.]+)'),
        'test_seconds': number(r'Total Test time \(real\) =\s*([\d.]+)'),
        'recipe_seconds': number(r'recipe_wall_seconds=([\d.]+)'),
        'passed': total - failed, 'failed': failed, 'total': total,
        'cache_hits': cache['direct_cache_hit'] + cache['preprocessed_cache_hit'],
        'cache_misses': cache['cache_miss'], 'cache_results': dict(cache),
        'failed_shards': re.findall(r'^\s*\d+ - (\S+) \(Failed\)', text, re.M),
        'failed_assertions': [line.strip() for line in text.splitlines() if ': ERROR:' in line],
    }


benchmarks = {phase: {tier: measurement(phase + '-' + tier)
                     for tier in ('dev', 'no-long-double', 'full')}
              for phase in ('before', 'after')}
assert all(item['failed'] == 0 for phase in benchmarks.values() for item in phase.values())
unfixed = {mode: measurement('optimized-unfixed-' + recipe)
           for mode, recipe in [('native', 'cpp-test'), ('no-long-double', 'cpp-test-no-long-double')]}
for item in unfixed.values():
    assert item['failed_shards'] == ['distribution_burr', 'distribution_invburr']
    assert len(item['failed_assertions']) == 2

inventory = {}
for path in sorted((root / 'univariateML-cpp/include/cppunivariate/distributions').glob('*/truth.json')):
    raw = path.read_bytes()
    table = json.loads(raw)
    inventory[path.parent.name] = {key: len(table.get(key, []))
                                  for key in ('parameters', 'values', 'quantiles', 'sampler_points')}
    inventory[path.parent.name]['sha256'] = hashlib.sha256(raw).hexdigest()

sources = ['.github/workflows/core.yaml', 'Justfile', 'dev/tools/test-changed.py',
           'univariateML-cpp/CMakeLists.txt', 'univariateML-cpp/tests/CMakeLists.txt',
           'univariateML-cpp/tests/test_tails.cpp',
           'univariateML-cpp/include/cppunivariate/special/tails.hpp']
for family in ('burr', 'invburr'):
    sources += ['univariateML-cpp/include/cppunivariate/distributions/' + family + '/test.cpp',
                'bindings/r/distributions/' + family + '/test.R']
summary = {
    'baseline_commit': '217ab8b533cac25ccc8eba8d66eccde1b376f0b4',
    'configurations': {
        'before': 'Original flags: c++17, default O0 except diagnostics at O2',
        'optimized_unfixed': 'g O2 gnu++17, assertions enabled (exploratory full replay)',
        'after': 'g O2 DNDEBUG gnu++17 (R optimization, assertion and language modes)',
    },
    'benchmarks': benchmarks, 'optimized_unfixed': unfixed,
    'truth_totals': {'tables': len(inventory), **{key: sum(item[key] for item in inventory.values())
                     for key in ('parameters', 'values', 'quantiles', 'sampler_points')}},
    'truth_tables': inventory,
    'source_sha256': {name: hashlib.sha256((root / name).read_bytes()).hexdigest() for name in sources},
}
(data / 'measurements.json').write_text(json.dumps(summary, indent=2) + '\n')
# Keep compact raw logs, not baseline source archives or binaries.
logs = [scratch / (phase + '-' + tier + '.log') for phase in ('before', 'after')
        for tier in ('dev', 'no-long-double', 'full')]
logs += list(scratch.glob('optimized-unfixed-*.log'))
logs += list(scratch.glob('*-last-test.log'))
logs += list(scratch.glob('final-*.log'))
for path in sorted(set(logs)):
    if 'ccache' in path.name:
        continue
    with gzip.open(data / (path.name + '.gz'), 'wb') as out:
        out.write(path.read_bytes())
print(json.dumps(summary['truth_totals']))
print(json.dumps({phase: {tier: {key: item[key] for key in
                 ('build_seconds', 'test_seconds', 'recipe_seconds', 'cache_hits', 'cache_misses')}
                 for tier, item in rows.items()} for phase, rows in benchmarks.items()}, indent=2))
