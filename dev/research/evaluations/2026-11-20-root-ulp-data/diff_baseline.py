"""Compare a fit-snapshot directory against a frozen snapshot."""
import argparse
from collections import Counter, defaultdict
import math
from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[3] / 'validation'))
from strict_common import load, number  # noqa: E402


def compare(baseline, candidate):
    before, after = load(baseline), load(candidate)
    scope = {key[0] for key in after}
    assert set(after) == {key for key in before if key[0] in scope}, 'case sets differ'
    totals = defaultdict(Counter)
    movements = defaultdict(lambda: (0, '', '', ''))
    likelihoods = defaultdict(float)
    transitions = []
    for (family, source, case), row in sorted(after.items()):
        old = before[family, source, case]
        assert row['input_sha256'] == old['input_sha256'], (family, source, case)
        a, b = old['result'], row['result'].copy()
        radius = b.pop('root_radius', None)
        totals[family]['cases'] += 1
        totals[family]['with radius'] += radius is not None
        totals[family]['changed'] += a != b
        totals[family]['estimates changed'] += a['estimates'] != b['estimates']
        for field in ('kind', 'reason', 'certified'):
            if a.get(field) != b.get(field):
                transitions.append((family, source, case, row['mask'], field, a.get(field), b.get(field)))
        for name in a['estimates'].keys() & b['estimates'].keys():
            previous, current = number(a['estimates'][name]), number(b['estimates'][name])
            if previous == current or not all(map(math.isfinite, (previous, current))):
                continue
            move = abs(current - previous) / abs(previous) if previous else math.inf
            movements[family] = max(movements[family], (move, source, case, name))
        previous, current = number(a['logLik']), number(b['logLik'])
        if all(map(math.isfinite, (previous, current))):
            likelihoods[family] = max(likelihoods[family], abs(current-previous)/max(1, abs(previous)))
    print('| Family | Cases | Changed | Coefficients changed | Max relative coefficient move | Max scaled logLik move | Radii |')
    print('|---|---:|---:|---:|---:|---:|---:|')
    for family, counts in sorted(totals.items()):
        move, source, case, name = movements[family]
        label = f'{move:.3g} ({source}/{case}, {name})' if move else '0'
        print(f"| {family} | {counts['cases']} | {counts['changed']} | {counts['estimates changed']} | "
              f"{label} | {likelihoods[family]:.3g} | {counts['with radius']} |")
    print()
    if transitions:
        for family, source, case, mask, field, old, new in transitions:
            print(f'- {family} {source}/{case} (mask {mask}) {field}: {old} → {new}')
    else:
        print('No change in outcome kind, reason or certification flag.')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('baseline')
    parser.add_argument('candidate')
    args = parser.parse_args()
    compare(args.baseline, args.candidate)
