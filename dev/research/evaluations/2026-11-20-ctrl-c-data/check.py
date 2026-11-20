"""Compare the cluster snapshots and check four held-scale Burr fits."""
import argparse
import json
from pathlib import Path
import sys

import mpmath as mp

EVALUATIONS = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(EVALUATIONS.parents[1] / 'validation'))
from strict_common import archive_data, load_one, number, snapshot_module


def compare(fresh, baseline):
    rows = load_one(fresh)
    old = load_one(baseline)
    families = {key[0] for key in rows}
    expected = {key for key in old if key[0] in families}
    assert rows.keys() == expected, (expected - rows.keys(), rows.keys() - expected)
    for family in sorted(families):
        keys = sorted(key for key in rows if key[0] == family)
        changed = []
        moves = []
        for key in keys:
            row, before = rows[key], old[key]
            for field in ('held', 'mask', 'input_sha256'):
                assert row[field] == before[field], (key, field)
            a, b = before['result'], row['result']
            if a != b:
                changed.append(key)
            if a['kind'] != b['kind'] or a['outcome'] != b['outcome']:
                print(f'{"/".join(key)} {row["held"]} '
                      f'{a["kind"]}/{a["outcome"]} -> {b["kind"]}/{b["outcome"]}')
            if a['kind'] == b['kind'] == 'ok':
                for name in a['estimates']:
                    u, v = (number(r['estimates'][name]) for r in (a, b))
                    error = abs(v-u)/abs(u) if u else abs(v)
                    moves.append((error, key[1], key[2], name))
        print(f'{family}: {len(keys)} compared, {len(changed)} changed, '
              f'maximum coefficient move {max(moves, default=None)}')
    return rows


def reference(x, scale, center, reflect=False):
    """Solve the strict concave scalar profile at 80 digits."""
    y = [(-1 if reflect else 1)*mp.log(mp.mpf(v)/scale) for v in x]
    n = len(y)

    def score(c):
        g = mp.fsum(mp.log1p(mp.exp(c*v)) for v in y)
        gp = mp.fsum(v/(1+mp.exp(-c*v)) for v in y)
        return n/c - n*gp/g + mp.fsum(v/(1+mp.exp(c*v)) for v in y)

    c = mp.findroot(score, (center*mp.mpf('.99'), center*mp.mpf('1.01')))
    k = n/mp.fsum(mp.log1p(mp.exp(c*v)) for v in y)
    loglik = mp.fsum(mp.log(k*c/mp.mpf(v)) - mp.log1p(mp.exp(-c*r))
                     - k*mp.log1p(mp.exp(c*r)) for v, r in zip(x, y))
    return k, c, loglik


def check_scale(rows):
    mp.mp.dps = 80
    archive = archive_data()
    passed = True
    for case in (688, 689, 9424, 9884):
        row = rows['burr', 'archive', f'{case:06d}']
        result = row['result']
        if result['kind'] != 'ok':
            print(f'{case:06d}: {result["kind"]}, {result["reason"]}')
            passed = False
            continue
        scale = mp.mpf(row['held']['scale'])
        k, c = (mp.mpf(number(result['estimates'][name]))
                for name in ('shape1', 'shape2'))
        stars = reference(archive[case], scale, c)
        errors = [abs(v/s-1) for v, s in zip((k,c), stars)]
        print(f'{case:06d}: relative errors '
              f'{", ".join(mp.nstr(e, 8) for e in errors)}, '
              f'certified={result["certified"]}')
        passed &= max(errors) < mp.mpf('1e-10')
    return passed


def check_betapr_move(rows, baseline):
    """Check the largest default Beta Prime coefficient move against its score root."""
    mp.mp.dps = 80
    snapshot = snapshot_module()
    meta, golden = snapshot.read('betapr')
    logs = [mp.log(mp.mpf(v)) for v in snapshot.classes(meta, golden)['ties']]
    key = 'betapr', 'class', 'ties'
    names = ('shape1', 'shape2', 'scale')
    new = [mp.mpf(number(rows[key]['result']['estimates'][name])) for name in names]
    old_row = load_one(baseline)[key]
    old = [mp.mpf(number(old_row['result']['estimates'][name])) for name in names]

    def score(p, q, log_scale):
        t = [v-log_scale for v in logs]
        n = len(t)
        return (n*(mp.digamma(p+q)-mp.digamma(p))-mp.fsum(mp.log1p(mp.exp(-v)) for v in t),
                n*(mp.digamma(p+q)-mp.digamma(q))-mp.fsum(mp.log1p(mp.exp(v)) for v in t),
                mp.fsum(q/(1+mp.exp(-v))-p/(1+mp.exp(v)) for v in t))

    p, q, log_scale = mp.findroot(score, (new[0], new[1], mp.log(new[2])))
    root = p, q, mp.exp(log_scale)
    for label, coefficients in (('baseline', old), ('updated', new)):
        errors = [mp.nstr(abs(v/s-1), 8) for v, s in zip(coefficients, root)]
        print(f'betapr class/ties {label} relative errors {", ".join(errors)}')


def fixed_scale_fixtures():
    mp.mp.dps = 80
    distributions = EVALUATIONS.parents[2] / 'univariateML-cpp/include/cppunivariate/distributions'
    for family in ('burr', 'invburr'):
        fixture = json.loads((distributions / family / 'fixed-scale-golden.json').read_text())
        values = reference(fixture['data'], mp.mpf(1), mp.mpf(fixture['estimates']['shape2']),
                           reflect=family == 'invburr')
        print(family, *(mp.nstr(v, 60) for v in values))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--fixed-scale-fixtures', action='store_true')
    parser.add_argument('snapshot', nargs='?')
    parser.add_argument('baseline', nargs='?')
    args = parser.parse_args()
    if args.fixed_scale_fixtures:
        fixed_scale_fixtures()
        sys.exit(0)
    if not args.snapshot or not args.baseline:
        parser.error("snapshot and baseline are both required")
    rows = compare(args.snapshot, args.baseline)
    check_betapr_move(rows, args.baseline)
    sys.exit(0 if check_scale(rows) else 1)
