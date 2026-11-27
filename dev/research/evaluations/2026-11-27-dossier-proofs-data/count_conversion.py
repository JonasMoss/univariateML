#!/usr/bin/env python3
"""Summarize detector grades and recover the original atlas review queue."""
import argparse
from collections import Counter, defaultdict
import csv
import io
import json
from pathlib import Path
import subprocess
import sys

import yaml

ROOT = Path(__file__).resolve().parents[4]
sys.path.insert(0, str(ROOT / 'dev/validation'))
from existence_keys import limit_groups

DIST = ROOT / 'univariateML-cpp/include/cppunivariate/distributions'
ATLAS = 'dev/research/existence-atlas/atlas.csv'


def previous_file(base, path):
    return subprocess.check_output(
        ['git', 'show', f'{base}:{path.relative_to(ROOT)}'], cwd=ROOT, text=True)


def groups(text):
    return dict(limit_groups(yaml.safe_load(text.split('---', 2)[1])))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--base', default='06b34bf3')
    args = parser.parse_args()
    original = defaultdict(list)
    for row in csv.DictReader(io.StringIO(previous_file(args.base, ROOT / ATLAS))):
        for record in json.loads(row['limits']):
            original[row['family'], record['index']].append(record)
    grades = Counter()
    conversions = Counter()
    modifiers = Counter()
    references = Counter()
    reviewed = []
    for path in sorted(DIST.glob('*/README.md')):
        old_groups = groups(previous_file(args.base, path))
        new_groups = groups(path.read_text(encoding='utf-8'))
        if new_groups.keys() - old_groups.keys():
            raise ValueError(f'Line up the added groups first for {path}')
        for scope in old_groups.keys() - {'limits'}:
            if scope not in new_groups:
                for old in old_groups[scope]:
                    modifiers[str(old.get('numerical', 'absent')).lower(), 'removed'] += 1
                continue
            if len(old_groups[scope]) != len(new_groups[scope]):
                raise ValueError(f'Line up {scope} first for {path}')
            for old, new in zip(old_groups[scope], new_groups[scope]):
                flag = str(old.get('numerical', 'absent')).lower()
                modifiers[flag, new.get('detect_firmness', 'no detector')] += 1
        before = old_groups['limits']
        after = new_groups['limits']
        if len(before) != len(after):
            raise ValueError(f'Line up the records first for {path}')
        for index, (old, new) in enumerate(zip(before, after), 1):
            family = path.parent.name
            grade = new.get('detect_firmness', 'path only')
            grades[grade] += 1
            conversions[str(old.get('numerical', 'absent')).lower(), grade] += 1
            for field in ('proof', 'detect_proof'):
                references[field] += bool(new.get(field))
            if (old.get('firmness') == 'proved' and not old.get('numerical') and
                    (old.get('detect') or old.get('status')) and
                    any(r['limit_proof']['status'] not in {'lean', 'written', 'cited'}
                        for r in original[family, index])):
                reviewed.append((family, index, grade))
    print('Final grades:', dict(sorted(grades.items())))
    print('Records with pointers:', dict(references))
    for (old, new), count in sorted(conversions.items()):
        print(f'Legacy numerical={old} -> {new}: {count}')
    for (old, new), count in sorted(modifiers.items()):
        print(f'Modifier numerical={old} -> {new}: {count}')
    print(f'Reviewed proved-path records with an open detector scope: {len(reviewed)}')
    for family, index, grade in reviewed:
        print(f'{family} {index}: {grade}')


if __name__ == '__main__':
    main()
