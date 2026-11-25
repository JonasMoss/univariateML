"""Replay archived public beta-prime coefficients on their original inputs."""
import gzip
import hashlib
import json
import math
from pathlib import Path

import mpmath as mp

EVALUATIONS = Path(__file__).resolve().parents[1]
ROOT = EVALUATIONS.parents[2]
NAMES = ('shape1', 'shape2', 'scale')
SNAPSHOTS = EVALUATIONS / '2026-11-15-fit-snapshot-data'
ARCHIVE = EVALUATIONS / '2026-11-07-fit-audit-data/cases.json.gz'


def read_rows(path):
    with gzip.open(path, 'rt') as source:
        yield from map(json.loads, source)


def exact_float(value):
    a, b = value.as_integer_ratio()
    return mp.mpf(a)/b


def input_hash(x, held):
    mask = sum(1 << i for i, name in enumerate(NAMES) if name in held)
    fields = [str(mask), str(len(x)), *(float(held.get(name, 0)).hex() for name in NAMES),
              *(float(v).hex() for v in x)]
    return hashlib.sha256((' '.join(fields)+'\n').encode()).hexdigest()[:16]


def scores(logx, coef):
    p, q, b = coef
    z = [x-mp.log(b) for x in logx]
    u = [1/(1+mp.exp(-t)) for t in z]
    return (p*(mp.digamma(p+q)-mp.digamma(p)-mp.fsum(mp.log1p(mp.exp(-t)) for t in z)/len(z)),
            q*(mp.digamma(p+q)-mp.digamma(q)-mp.fsum(mp.log1p(mp.exp(t)) for t in z)/len(z)),
            (p+q)*mp.fsum(u)/len(u)-p)


def solve(x, held, actual):
    logx = [mp.log(exact_float(float(v))) for v in x]
    free = [i for i, name in enumerate(NAMES) if name not in held]
    public = [exact_float(v) for v in actual]

    def assemble(z):
        fit = public.copy()
        for i, value in zip(free, z):
            fit[i] = mp.exp(value)
        return fit

    def score(*z):
        full = scores(logx, assemble(z))
        return [full[i] for i in free]

    def jacobian(*z):
        coef = assemble(z)
        p,q,b = coef
        node = [1/(1+mp.exp(mp.log(b)-x)) for x in logx]
        u = mp.fsum(node)/len(node)
        v = mp.fsum(1-f for f in node)/len(node)
        u1 = mp.fsum(f*(1-f) for f in node)/len(node)
        gp,gq,_ = scores(logx,coef)
        t = mp.polygamma(1,p+q)
        h = ((gp+p*p*(t-mp.polygamma(1,p)),p*q*t,-p*v),
             (p*q*t,gq+q*q*(t-mp.polygamma(1,q)),q*u),
             (-p*v,q*u,-(p+q)*u1))
        return mp.matrix([[h[i][j] for j in free] for i in free])

    start = [mp.log(public[i]) for i in free]
    root = mp.findroot(score,start,J=jacobian,tol=mp.mpf('1e-76'),maxsteps=50)
    hessian = jacobian(*root)
    assert max(mp.eigsy(hessian, eigvals_only=True)) < 0
    ref = assemble(root)
    errors = [abs(public[i]/ref[i]-1) for i in free]
    return ref, errors, max(abs(t) for t in score(*root))


def main():
    archive = {f"{c['index']:06}": c for c in json.loads(gzip.decompress(ARCHIVE.read_bytes()))
               if c['family'] == 'betapr'}
    golden_path = ROOT / 'univariateML-cpp/include/cppunivariate/distributions/betapr/golden.json'
    golden = json.loads(golden_path.read_text())
    all_rows = {
        mode: {r['case']: r for r in read_rows(SNAPSHOTS / f'{mode}.jsonl.gz')
               if r['family']=='betapr' and r['result']['kind']=='ok'
               and r['source'] in ('archive', 'golden') and r['mask'] != 7}
        for mode in ('native','double')
    }
    common = [r for label, r in all_rows['native'].items()
              if label in all_rows['double']]
    print(json.dumps({'common_successful_counts_by_mask':
                      [sum(r['mask']==m for r in common) for m in range(7)]}),
          flush=True)
    cases = ['golden']
    for m in range(7):
        group = [r for r in common if r['mask']==m and r['case']!='golden']
        if not group:
            continue
        # Stress the weak shape coordinate and include an ordinary control
        # on each one-shape-held mask.
        def size(r):
            return max(float.fromhex(r['result']['estimates'][name]) for name in NAMES[:2])
        cases.append(max(group,key=size)['case'])
        if m in (1,2):
            cases.append(min(group,key=size)['case'])
        if m==4:
            cases.extend(['000243','000249'])
    for label in cases:
        for mode, rows in all_rows.items():
            row = rows[label]
            data = golden['data'] if label=='golden' else archive[label]['data']
            held = {k:float.fromhex(v) if isinstance(v,str) else v for k,v in row['held'].items()}
            assert input_hash(data,held) == row['input_sha256']
            values = [float.fromhex(row['result']['estimates'][k]) for k in NAMES]
            assert all(values[NAMES.index(k)]==v for k,v in held.items())
            # Raw digamma and log1p score cancellation can consume over
            # 190 decimal places on the unequal-shape fixed-scale fixture.
            guard = max(0,int(math.log10(max(values))))
            with mp.workdps(95+guard):
                low, _, _ = solve(data,held,values)
            with mp.workdps(135+guard):
                ref, errors, residual = solve(data,held,values)
                assert max(abs(a/b-1) for a,b in zip(low,ref)) < mp.mpf('1e-50')
                assert residual < mp.mpf('1e-65')
                print(json.dumps(dict(case=label, mode=mode, n=len(data), held=row['held'],
                                      certified=row['result']['certified'],
                                      precisions=[95+guard,135+guard],
                                      public=[v.hex() for v in values],
                                      reference=[mp.nstr(v,31) for v in ref],
                                      errors=[mp.nstr(v,9) for v in errors],
                                      meets_tol=max(errors)<=mp.mpf('1e-10'))),
                      flush=True)


if __name__=='__main__':
    main()
