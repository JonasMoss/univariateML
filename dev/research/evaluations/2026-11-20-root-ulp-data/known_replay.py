"""Extract the TASK-214.31 root verdicts from a cert-replay.py records file."""
import argparse
import json
from pathlib import Path

ROOT = Path(__file__).resolve().parents[4]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('records', type=Path)
    args = parser.parse_args()
    known = json.loads((ROOT / 'dev/validation/strict-known.json').read_text())['known']
    targets = {key for key, value in known.items()
               if key.startswith('cert-replay:root|') and value['ref'] == 'TASK-214.31'}
    found = {}
    with args.records.open() as records:
        for line in records:
            record = json.loads(line)
            suffix = '|'.join(record[name] for name in ('mode', 'family', 'source', 'case'))
            key = 'cert-replay:root|' + suffix
            if key in targets:
                root = next((c for c in record['comparisons'] if c['name'] == 'root'), None)
                if root:
                    found[key] = (record['mask'], root['verdict'], root['rigor'])
    print('| Key | Mask | Verdict | Evidence |')
    print('|---|---:|---|---|')
    for key in sorted(targets):
        mask, verdict, rigor = found.get(key, ('', 'missing', ''))
        label = key.replace('|', r'\|')
        print(f"| {label} | {mask} | {verdict} | {rigor} |")
    return int(sum(verdict == 'holds' for _, verdict, _ in found.values()) != len(targets))


if __name__ == '__main__':
    raise SystemExit(main())
