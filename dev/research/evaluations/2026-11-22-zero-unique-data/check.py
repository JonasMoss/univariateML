#!/usr/bin/env python3
"""Check the additive case inventory and compare fresh snapshots with frozen ones."""
import collections
import gzip
import hashlib
import json
from pathlib import Path
import subprocess
import sys
import types

ROOT = Path(__file__).resolve().parents[4]
sys.path.insert(0, str(ROOT/'dev/validation'))
from flat_maxima import CASES
from strict_common import load, snapshot_module

BASELINES = ROOT/'dev/research/evaluations/2026-11-15-fit-snapshot-data'
SCRATCH = ROOT/'build/scratch/zero-unique'
fs = snapshot_module()
PRE_CHANGE = '1cfbc3dd'


def old_driver():
    path = ROOT/'dev/validation/fit-snapshot.py'
    text = subprocess.check_output(['git', 'show', f'{PRE_CHANGE}:{path.relative_to(ROOT)}'],
                                   cwd=ROOT, text=True)
    old = types.ModuleType('snapshot_before_rule')
    old.__file__ = str(path)
    exec(compile(text, str(path), 'exec'), vars(old))
    return old


def inventory(routes, stored):
    ids = {v['id']: k for k, v in fs.ROUTES.items()} if routes else {k: k for k in fs.families()}
    archive = collections.defaultdict(list)
    for row in json.loads(gzip.decompress(fs.ARCHIVE.read_bytes())):
        archive[row['family']].append(row)
    frozen = load(stored)
    new = set()
    old = old_driver()
    fs.ROUTE_MODE = old.ROUTE_MODE = routes
    for family, ident in ids.items():
        meta, _ = fs.read(ident)
        names = [p['name'] for p in meta['params']]
        all_cases = list(fs.cases(ident, archive))
        old_cases = list(old.cases(ident, archive))
        legacy = [case for case in all_cases if case['case'] not in CASES.get(ident, {})]
        signature = lambda case: (case['source'], case['case'], fs.line(case, names))
        assert list(map(signature, legacy)) == list(map(signature, old_cases)), family
        for case in all_cases:
            source, label = case['source'], case['case']
            key = family, source, label
            if key not in frozen:
                new.add(key)
                continue
            row = frozen.pop(key)
            digest = hashlib.sha256(fs.line(case, names).encode()).hexdigest()[:16]
            assert row['input_sha256'] == digest, key
    assert not frozen, sorted(frozen)
    fs.ROUTE_MODE = False
    expected = {(family, 'class', label) for family, ident in ids.items()
                for label in CASES.get(ident, {})}
    assert new == expected, (sorted(new - expected), sorted(expected - new))
    return expected


def comparison(current, stored, expected, *, complete=True, extra=None):
    snapshots = f'{current},{extra}' if extra else current
    frozen, fresh = load(stored), load(snapshots)
    new = fresh.keys() - frozen.keys()
    if complete:
        assert new == expected, (sorted(new - expected), sorted(expected - new))
    else:
        assert new <= expected, sorted(new - expected)
    changed = []
    for key in sorted(fresh.keys() & frozen.keys()):
        before, after = frozen[key], fresh[key]
        for field in ('family', 'source', 'case', 'mask', 'held', 'n', 'input_sha256'):
            assert before[field] == after[field], (key, field)
        fields = set(before['result']) | set(after['result'])
        diff = sorted(f for f in fields if before['result'].get(f) != after['result'].get(f))
        if diff:
            changed.append((key, diff))
    print(current.name, 'existing:', len(fresh) - len(new), 'new:', len(new), 'changed:', len(changed))
    for key, diff in changed:
        print('  existing result difference', '|'.join(key), ','.join(diff))
    return new


def main():
    for mode in ('native', 'double'):
        base = BASELINES/f'{mode}.jsonl.gz'
        routes = BASELINES/f'routes-{mode}.jsonl.gz'
        expected = inventory(False, base)
        route_expected = inventory(True, routes)
        public_extra = f'{SCRATCH}/extra-{mode},{SCRATCH}/tlambda-{mode}'
        new = comparison(SCRATCH/f'current-{mode}', base, expected, extra=public_extra)
        new |= comparison(SCRATCH/f'routes-{mode}', routes, route_expected,
                          extra=SCRATCH/f'routes-extra-{mode}')
        print(mode, 'all new keys')
        for key in sorted(new):
            print('  '+'|'.join(key))
    comparison(SCRATCH/'native', BASELINES/'native.jsonl.gz', expected, complete=False)


if __name__ == '__main__':
    main()
