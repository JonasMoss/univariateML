"""Run with PYTHONPATH=dev/validation from repository root after build.sh."""
import json
import math
from pathlib import Path
from functools import lru_cache
import subprocess
import sys

import mpmath as mp
from quality.truth_reference import gamma

mp.mp.dps = 80
DATA = Path(__file__).resolve().parent
BIN = Path("build/scratch/pged")
SHAPES = sorted({10 ** (k / 4) for k in range(25)} | {999999.})
AFFINE = [(0., 1.), (.3, 1.7), (-1e100, 1e100), (1e-100, 1e-100)]
POWERS = ('1e-40', '.001', '.1', '.5', '1', '2', '5', '20', '100', '400',
          '680', '700', '710', '730', '760', '1000', '1e6')
PROBS = ('1e-300', '1e-100', '1e-12', '.000001', '.01', '.25', '.49',
         '.51', '.75', '.999999', '0.9999999999999999')
LOGPROBS = (-10000., -800., -703., -690., -230., -27., -5., -.7, -.69, -.01, -1e-10, -1e-100)


@lru_cache(maxsize=None)
def logtails(x, mean, scale, nu):
    power = (abs(x - mean) / scale) ** nu
    small = gamma(1 / nu, power)[1] - mp.log(2)
    big = mp.log1p(-mp.exp(small))
    return (small, big) if x < mean else (big, small)


@lru_cache(maxsize=None)
def scale_for(sd, nu):
    return mp.mpf(sd) * mp.exp((mp.loggamma(1 / nu) - mp.loggamma(3 / nu)) / 2)


def inverse(lp, nu):
    small = lp <= -mp.log(2)
    mass = mp.log(2) + (lp if small else mp.log(-mp.expm1(lp)))
    lo, hi = mp.mpf(-1024), mp.mpf(64)
    def log_upper(w):
        return gamma(1 / nu, mp.exp(w * nu))[1]
    if not log_upper(lo) > mass > log_upper(hi):
        raise ArithmeticError(f"unbracketed quantile for log(p)={lp}, nu={nu}")
    for _ in range(90):
        mid = (lo + hi) / 2
        if log_upper(mid) > mass:
            lo = mid
        else:
            hi = mid
    if hi - lo > mp.mpf('1e-24'):
        raise ArithmeticError("quantile log-radius bracket exceeds its budget")
    return mp.exp((lo + hi) / 2), small


def probability(lp, log):
    return lp if log else mp.exp(lp)


def observations():
    for shape in SHAPES:
        nu = mp.mpf(shape)
        base = mp.exp((mp.loggamma(1 / nu) - mp.loggamma(3 / nu)) / 2)
        qs = []
        for log, inputs in ((False, map(float, PROBS)), (True, LOGPROBS)):
            for p in inputs:
                lp = mp.mpf(p) if log else mp.log(mp.mpf(p))
                qs.append((p, log, *inverse(lp, nu)))
        for mean, sd in AFFINE:
            m, s = mp.mpf(mean), mp.mpf(sd) * base
            for sign in (-1, 1):
                for power in POWERS:
                    x = float(m + sign * s * mp.mpf(power) ** (1 / nu))
                    t = mp.mpf(x)
                    logs = logtails(t, m, s, nu)
                    ld = -mp.log(2 * s) - mp.loggamma(1 + 1 / nu) - (abs(t - m) / s) ** nu
                    for log in (False, True):
                        yield ('d', x, mean, sd, shape, True, log), probability(ld, log)
                        for lower in (False, True):
                            yield ('p', x, mean, sd, shape, lower, log), probability(logs[not lower], log)
            for p, log, radius, small in qs:
                for lower in (False, True):
                    q = m + (-1 if small == lower else 1) * s * radius
                    yield ('q', p, mean, sd, shape, lower, log), q


def error(got, exact, relative, q=False):
    # Use a lattice check for positive subnormals, rather than scaling
    # their error by the reference. Compare zero separately.
    rounded = float(exact)
    if relative and not q and 0 < rounded < sys.float_info.min:
        return None
    if not math.isfinite(got) or not math.isfinite(rounded) or rounded == 0:
        return 0. if got == rounded else math.inf
    denom = abs(exact) if relative else max(1, abs(exact))
    return float(abs(mp.mpf(got) - exact) / denom)


def summarize(rows):
    input_text = ''.join(f"{op} {x:.17g} {m:.17g} {s:.17g} {n:.17g} {int(l)} {int(log)}\n"
                         for (op, x, m, s, n, l, log), _ in rows)
    result = {}
    for binary in sorted(BIN.glob('*-*')):
        if not binary.is_file():
            continue
        output = subprocess.check_output([binary], input=input_text, text=True).splitlines()
        summary = {}
        def record(key, args, got, expected, relative, tol, q=False):
            op, x, m, s, n, lower, log = args
            group = summary.setdefault(f"{key}/{('log' if log else 'natural')}",
                                       dict(count=0, subnormal=0, subnormal_over=0, nan=0, over=0,
                                            max_error=0., max_finite_error=0., nonfinite=0,
                                            worst=None, by_shape={}, unit_by_shape={}, examples=[],
                                            subnormal_examples=[], nan_examples=[]))
            group['count'] += 1
            if math.isnan(got):
                group['nan'] += 1
                if len(group['nan_examples']) < 8:
                    group['nan_examples'].append([*args, float(expected)])
            e = error(got, expected, relative, q)
            if e is None:
                rounded = float(expected)
                # Include one grid spacing for rounding and the usual
                # evaluation budget before conversion to the grid.
                ok = (math.isfinite(got) and got > 0
                      and abs(mp.mpf(got) - expected) <= mp.mpf(tol)*expected + math.ulp(0.))
                group['subnormal'] += 1
                group['subnormal_over'] += not ok
                group['over'] += not ok
                group['nonfinite'] += not math.isfinite(got)
                if not ok and len(group['subnormal_examples']) < 8:
                    group['subnormal_examples'].append([*args, got, rounded])
                return
            group['over'] += e > tol
            if math.isfinite(e):
                group['max_finite_error'] = max(group['max_finite_error'], e)
            else:
                group['nonfinite'] += 1
            if e > tol and len(group['examples']) < 8:
                group['examples'].append([*args, got, float(expected)])
            if e > group['max_error']:
                group.update(max_error=e, worst=[*args, got, float(expected)])
            group['by_shape'][str(n)] = max(group['by_shape'].get(str(n), 0.), e)
            if m == 0 and s == 1:
                group['unit_by_shape'][str(n)] = max(group['unit_by_shape'].get(str(n), 0.), e)
        for (args, expected), line in zip(rows, output, strict=True):
            op, x, m, s, n, lower, log = args
            got, *other = map(float, line.split())
            if op == 'q':
                record(op, args, got, expected, True, 1e-9, q=True)
                shape = mp.mpf(n)
                scale = scale_for(s, shape)
                exact = probability(logtails(mp.mpf(got), mp.mpf(m), scale, shape)[not lower], log)
                record('p(q)', args, other[0], exact, not log, 1e-10)
                record('roundtrip', args, other[0], mp.mpf(x), not log, 1e-10)
                record('lattice_roundtrip', args, float(probability(
                    logtails(mp.mpf(float(expected)), mp.mpf(m), scale, shape)[not lower], log)),
                    mp.mpf(x), not log, 1e-10)
            else:
                record(op, args, got, expected, not log, 1e-10)
        result[binary.name] = summary
        print(binary.name, {k: (f"{v['max_error']:.3g}", v['over']) for k, v in summary.items()}, flush=True)
    return result, input_text


def json_safe(value):
    if isinstance(value, dict):
        return {k: json_safe(v) for k, v in value.items()}
    if isinstance(value, (tuple, list)):
        return [json_safe(v) for v in value]
    if isinstance(value, float) and not math.isfinite(value):
        return str(value)
    return value


if __name__ == '__main__':
    result, probes = summarize(list(observations()))
    (DATA / 'accuracy.json').write_text(json.dumps(json_safe(result), indent=2, allow_nan=False) + '\n')
    (BIN / 'inputs').write_text(probes)
