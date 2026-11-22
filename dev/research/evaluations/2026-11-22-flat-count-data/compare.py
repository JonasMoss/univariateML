"""Compare the five fresh family snapshots with branch-base and frozen fits."""
import argparse
from pathlib import Path
import sys

ROOT = Path(__file__).resolve().parents[4]
sys.path.insert(0, str(ROOT / 'dev/validation'))
from flat_maxima import CASES
from strict_common import load

FAMILIES = {'binom', 'bbinom', 'nhyper', 'hyper', 'cmb'}


def selected(path):
    return {key: row for key, row in load(str(path)).items() if key[0] in FAMILIES}


def outcome(result):
    value = result.get('outcome') or {}
    if value.get('kind') != 'estimate':
        return f'{value.get("kind")}/{value.get("reason")}'
    unique = 'unique' if value['unique'] else 'nonunique'
    certification = 'certified' if value['certified'] else 'uncertified'
    return f'{certification} {unique} estimate'


def compare(label, old, new):
    print(f'{label}: {len(old)} before, {len(new)} after')
    for key in sorted(new.keys() - old.keys()):
        family, source, case = key
        if source != 'class' or case not in CASES.get(family, {}):
            raise ValueError(f'unexpected new record {key}')
    if old.keys() - new.keys():
        raise ValueError(f'missing records: {sorted(old.keys() - new.keys())}')
    changes = 0
    for key in sorted(old.keys() & new.keys()):
        if old[key]['input_sha256'] != new[key]['input_sha256']:
            raise ValueError(f'input changed for {key}')
        a, b = old[key]['result'], new[key]['result']
        fields = sorted(k for k in a.keys() | b.keys() if a.get(k) != b.get(k))
        if fields:
            changes += 1
            print(f'  {" ".join(key)} | {outcome(a)} -> {outcome(b)} | {", ".join(fields)}')
    print(f'{changes} changed, {len(new.keys() - old.keys())} added flat cases\n')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('before', type=Path)
    parser.add_argument('after', type=Path)
    parser.add_argument('frozen', type=Path)
    args = parser.parse_args()
    before, after, frozen = map(selected, (args.before, args.after, args.frozen))
    compare('Frozen to branch base', frozen, before)
    compare('Branch base to job', before, after)
    compare('Frozen to job', frozen, after)


if __name__ == '__main__':
    main()
