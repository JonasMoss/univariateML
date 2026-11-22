#!/usr/bin/env python3
"""Evaluate the replay cases through the strict density consumer in both modes."""
import argparse
import json
import math
from pathlib import Path
import runpy
import sys

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('records', type=Path)
parser.add_argument('--double', action='store_true')
parser.add_argument('--baseline', action='store_true')
args = parser.parse_args()
root = Path(__file__).resolve().parents[4]
validation = root/'dev/validation'
sys.path.insert(0, str(validation))
density = runpy.run_path(str(validation/'fit-density.py'))
table = density['laws']()
exe, index = density['build'](table, args.double)
probe = density['fs'].Probe(exe)
try:
    for record in map(json.loads, args.records.read_text().splitlines()):
        family = record['family']
        label, model, values, reported, _, own = density['items'](record, family, table)[0]
        law, data, endpoint, factor = density['payload_data'](model, values, record['data'])
        p, missing = density['coefficients'](law, table, values, record['held'], data, own)
        assert not missing, missing

        def summed(observations):
            text = ' '.join(float(v).hex() for v in p + observations)
            result = probe.ask(f'{index[law]} {len(p)} {len(observations)} {text}\n', 30)
            terms = [density['number'](v) for v in result['terms']]
            return math.fsum(term + (factor if v == endpoint else 0.)
                                        for term, v in zip(terms, observations))

        total = summed(data)
        same = abs(total-reported) <= 1e-10*max(1., abs(reported))
        if args.baseline and family in ('fpareto', 'pareto2'):
            assert not same, record
        elif not args.baseline:
            assert same, record
            for candidate in record['result']['candidates']:
                if candidate['model'] != model or candidate['estimates'] != record['result']['limit_estimates']:
                    continue
                lo, hi = (density['number'](candidate[k]) for k in ('loglik_lower', 'loglik_upper'))
                margin = 1e-10*max(1., abs(lo), abs(hi))
                assert lo-margin <= total <= hi+margin, candidate
        print(json.dumps(dict(family=family, held=record['held'], model=model,
                              reported=reported, evaluated=total if math.isfinite(total) else str(total), matches=same)))
finally:
    probe.close()
