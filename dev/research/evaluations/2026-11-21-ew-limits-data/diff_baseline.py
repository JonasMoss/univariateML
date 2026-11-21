"""Compare family snapshot parts to the frozen, all-family baseline."""
import argparse
import json
from pathlib import Path
import sys

sys.path.insert(0, 'dev/validation')
from strict_common import load_one, number


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('candidate', type=Path)
    parser.add_argument('baseline', type=Path)
    args = parser.parse_args()
    new = load_one(args.candidate)
    old = load_one(args.baseline)
    for family in sorted({key[0] for key in new}):
        keys = sorted(key for key in new if key[0] == family)
        assert set(keys) == {key for key in old if key[0] == family}
        changed = []
        kinds = []
        flags = []
        maximum = (0., None, None)
        for key in keys:
            a, b = old[key], new[key]
            assert a['input_sha256'] == b['input_sha256']
            before, after = a['result'], b['result']
            if before == after:
                continue
            changed.append(key[1:])
            if before['kind'] != after['kind']:
                kinds.append([*key[1:], before['kind'], after['kind']])
            if before['kind'] != 'ok' or after['kind'] != 'ok':
                continue
            if before['certified'] != after['certified']:
                flags.append([*key[1:], before['certified'], after['certified']])
            for name, old_value in before['estimates'].items():
                x = number(old_value)
                y = number(after['estimates'][name])
                delta = 0. if x == y else abs(y - x) / (abs(x) if x else 1.)
                if delta > maximum[0]:
                    maximum = delta, key[1:], name
        print(json.dumps(dict(family=family, compared=len(keys), changed=len(changed),
                              max_relative=maximum, kind_changes=kinds, certification_changes=flags)))


if __name__ == '__main__':
    main()
