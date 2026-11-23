"""Check the replay and independently sum the double-Pareto log density.

Run from the repository root with baseline snapshots of the added fixtures,
the fresh snapshots and the corresponding fit-density.py --records files.
"""
import argparse
import json
import math
from pathlib import Path

import mpmath as mp


def snapshots(folder):
    for path in sorted(Path(folder).glob('*.jsonl')):
        for line in path.read_text().splitlines()[1:]:
            row = json.loads(line)
            if row['case'].startswith(('threshold-', 'zero-tied-')):
                yield (row['family'], row['case']), row


def densities(path, mode):
    with open(path) as stream:
        for line in stream:
            row = json.loads(line)
            if row['mode'] == mode and row['case'].startswith(('threshold-', 'zero-tied-')):
                if math.isinf(row['relative']):
                    row['relative'] = 'inf'
                yield (row['family'], row['case'], row['item']), row


def check_double_pareto(row, density):
    with mp.workdps(100):
        kink, left, right, offset = (
            float.fromhex(row['result']['limit_estimates'][name])
            for name in ('kink', 'left_rate', 'right_rate', 'min'))
        terms = []
        for x in row['data']:
            y = mp.mpf(float(x - offset))
            ratio = y / kink
            # Definition in the original excess coordinate, with total
            # probability masses right/(left+right) and left/(left+right).
            coefficient = mp.mpf(left) * right / (mp.mpf(left) + right) / kink
            exponent = mp.mpf(left) - 1 if y < kink else -mp.mpf(right) - 1
            terms.append(mp.log(coefficient * mp.power(ratio, exponent)))
        reference = mp.fsum(terms)
        assert abs(reference - density) <= 1e-12 * abs(reference)
        return float(reference)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('mode', choices=('native', 'double'))
    parser.add_argument('before_snapshot')
    parser.add_argument('after_snapshot')
    parser.add_argument('before_density')
    parser.add_argument('after_density')
    args = parser.parse_args()
    before = dict(snapshots(args.before_snapshot))
    after = dict(snapshots(args.after_snapshot))
    old = dict(densities(args.before_density, args.mode))
    new = dict(densities(args.after_density, args.mode))
    assert before.keys() == after.keys()
    checked = set()
    for key, row in after.items():
        previous = before[key]['result']
        result = row['result']
        expected_kind = 'budget' if key[1] == 'threshold-exp-budget' else 'limit'
        assert previous['kind'] == result['kind'] == expected_kind, (key, previous, result)
        assert previous['outcome'] == result['outcome'], key
        assert previous['core_status'] == result['core_status'], key
        if previous['limit_loglik'] != 'nan':
            assert previous['limit_loglik'] == result['limit_loglik'], key
        for item, record in new.items():
            if item[:2] != key:
                continue
            assert record['pass'], item
            checked.add(key)
            out = dict(family=key[0], case=key[1], item=item[2],
                       law=record['law'], before=old.get(item), after=record)
            if item[2] == 'limit':
                out['before_payload'] = previous['model'], previous['limit_estimates']
                out['after_payload'] = result['model'], result['limit_estimates']
            if record['law'] == 'unavailable:double-pareto':
                out['mpmath'] = check_double_pareto(row, record['summed'])
            print(json.dumps(out, sort_keys=True, allow_nan=False))
    assert checked == after.keys(), after.keys() - checked


if __name__ == '__main__':
    main()
