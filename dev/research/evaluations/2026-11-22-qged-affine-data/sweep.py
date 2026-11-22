"""Run from the worktree root with PYTHONPATH=dev/validation."""
import importlib.util
import json
import math
from pathlib import Path
import struct
import subprocess
import sys

import mpmath as mp

DATA = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location(
    'pged_sweep', DATA.parent / '2026-11-22-pged-tail-data/sweep.py')
prior = importlib.util.module_from_spec(spec)
spec.loader.exec_module(prior)
mp.mp.dps = 100


def lattice(x):
    bits = struct.unpack('>Q', struct.pack('>d', abs(x)))[0]
    return -bits if x < 0 else bits


def references(fallback=False):
    shapes = (1., 50., 999999., 1e6) if fallback else prior.SHAPES
    affine = ((1e200, 1e200), (1e-200, 1e-200)) if fallback else prior.AFFINE
    probs = ('1e-300', '.25') if fallback else prior.PROBS
    logprobs = (-10000., -5.) if fallback else prior.LOGPROBS
    for nu in shapes:
        for log, ps in ((False, map(float, probs)), (True, logprobs)):
            for p in ps:
                lp = mp.mpf(p) if log else mp.log(mp.mpf(p))
                radius, small = prior.inverse(lp, mp.mpf(nu))
                for mean, sd in affine:
                    m, s = mp.mpf(mean), prior.scale_for(sd, mp.mpf(nu))
                    for lower in (False, True):
                        distance = s * radius
                        q = m + (-1 if small == lower else 1) * distance
                        rounded = float(q)
                        lo = rounded if rounded <= q else math.nextafter(rounded, -math.inf)
                        hi = rounded if rounded >= q else math.nextafter(rounded, math.inf)
                        # Check that the inherited inverse's log-radius bracket
                        # locates both neighbours and selects the same nearest one.
                        margin = distance * mp.expm1(mp.mpf('1e-24'))
                        if not (lo < q-margin and q+margin < hi
                                and float(q-margin) == float(q+margin) == rounded):
                            raise ArithmeticError(f'unresolved lattice near {q}')
                        yield (p, mean, sd, nu, lower, log), q, lo, hi


def evaluate(binary, rows, inputs):
    output = subprocess.check_output([binary], input=inputs, text=True).splitlines()
    result = {}
    for (args, q, lo, hi), line in zip(rows, output, strict=True):
        p, mean, sd, nu, lower, log = args
        x, evaluated, *route = map(float, line.split())
        fast = bool(route and route[0])
        m, n = mp.mpf(mean), mp.mpf(nu)
        s = prior.scale_for(sd, n)

        def cdf(at):
            lp = prior.logtails(mp.mpf(at), m, s, n)[not lower]
            return lp if log else mp.exp(lp)

        p = mp.mpf(p)
        denom = max(1, abs(p)) if log else p
        ideal, at_x = cdf(float(q)), cdf(x)
        signed = {
            'nearest_rounding': (ideal-p)/denom,
            'reconstruction': (at_x-ideal)/denom,
            'cdf_evaluation': (evaluated-at_x)/denom,
        }
        metrics = {
            'neighbor_gap': abs(cdf(hi)-cdf(lo))/denom,
            'nearest_rounding': abs(signed['nearest_rounding']),
            'reconstruction': abs(signed['reconstruction']),
            'oracle_roundtrip': abs(at_x-p)/denom,
            'pged_roundtrip': abs(evaluated-p)/denom,
            'pged_at_x': abs(evaluated-at_x)/(max(1, abs(at_x)) if log else at_x),
            'ulp_distance': abs(lattice(x)-lattice(float(q))),
            'coordinate_relative': abs(mp.mpf(x)-q)/abs(q),
        }
        if route and metrics['ulp_distance'] > (9 if fast else 1):
            raise ArithmeticError(f'quantile outside route allowance: {args}, {metrics}')
        if route and metrics['coordinate_relative'] > 1e-9:
            raise ArithmeticError(f'quantile outside relative tolerance: {args}, {metrics}')
        scale = 'log' if log else 'natural'
        groups = [scale, 'shape>=1000' if nu >= 1000 else 'shape<1000']
        if route:
            groups.append(f'{"fast" if fast else "fallback"}/{scale}')
        for key in groups:
            group = result.setdefault(key, {'count': 0, 'over_three_ulps': 0, 'maxima': {}})
            group['count'] += 1
            if route:
                group.setdefault('fast_count', 0)
                group.setdefault('fast_neighbour_count', 0)
                group['fast_count'] += fast
                group['fast_neighbour_count'] += fast and metrics['ulp_distance'] == 1
            group['over_three_ulps'] += metrics['ulp_distance'] > 3
            for name, value in metrics.items():
                current = group['maxima'].get(name)
                if current is None or value > current['value']:
                    group['maxima'][name] = dict(value=value if isinstance(value, int) else float(value),
                                                  input=args, returned=x, rounded=float(q))
                    if name == 'pged_roundtrip':
                        group['maxima'][name].update(
                            signed_split={k: float(v) for k, v in signed.items()},
                            ulp_distance=metrics['ulp_distance'],
                            neighbor_gap=float(metrics['neighbor_gap']))
    return result


if __name__ == '__main__':
    fallback = '--fallback' in sys.argv
    rows = list(references(fallback))
    inputs = ''.join(f'q {p:.17g} {m:.17g} {s:.17g} {n:.17g} {int(lower)} {int(log)}\n'
                     for (p, m, s, n, lower, log), *_ in rows)
    for variant in (arg for arg in sys.argv[1:] if arg != "--fallback"):
        for mode in ('native', 'double'):
            name = f'{variant}-{mode}'
            result = evaluate(Path('build/scratch/qged') / name, rows, inputs)
            suffix = '-fallback' if fallback else ''
            if fallback and any(group.startswith('fast/') for group in result):
                raise ArithmeticError('supplemental case did not use the fallback')
            (DATA / f'{name}{suffix}.json').write_text(json.dumps(result, indent=2, allow_nan=False)+'\n')
            print(name, {group: {k: v['value'] for k, v in detail['maxima'].items()}
                         for group, detail in result.items()}, flush=True)
