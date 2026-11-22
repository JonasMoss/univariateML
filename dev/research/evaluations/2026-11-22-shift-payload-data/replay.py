#!/usr/bin/env python3
"""Replay the C++ binding-neutral payloads using the frozen-snapshot probe.

Run with the single-slot runner from a source tree (or pass its path).
"""
import argparse
import importlib.util
import json
from pathlib import Path

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('source', type=Path)
parser.add_argument('work', type=Path)
parser.add_argument('--double', action='store_true')
args = parser.parse_args()
source, work = args.source.resolve(), args.work.resolve()
work.mkdir(parents=True, exist_ok=True)
spec = importlib.util.spec_from_file_location('fit_snapshot', source/'dev/validation/fit-snapshot.py')
fs = importlib.util.module_from_spec(spec)
spec.loader.exec_module(fs)

cases = [
    ('fpareto', [4]*8, dict(min=2, shape2=2, shape3=2)),
    ('pareto2', list(range(3, 13)), dict(min=2)),
    ('pareto4', [3, 3, 4, 5], dict(min=2, scale=1)),
    ('fpareto', [3, 3, 2.5, 2.25], dict(min=2, shape1=1, scale=1)),
]
executables = {family: fs.build(family, work, args.double, fs.boost_include())
               for family in dict.fromkeys(case[0] for case in cases)}
for family, x, held in cases:
    meta, _ = fs.read(family)
    names = [p['name'] for p in meta['params']]
    mask = sum(1 << i for i, name in enumerate(names) if name in held)
    probe = fs.Probe(executables[family])
    try:
        r = probe.ask(fs.line(dict(mask=mask, data=x, held=held), names), 30)
    finally:
        probe.close()
    assert r['kind'] == 'limit', r
    print(json.dumps(dict(family=family, data=x, held=held, result=r)))
