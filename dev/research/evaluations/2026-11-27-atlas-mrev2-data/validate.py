#!/usr/bin/env python3
"""Validate the atlas review CSV against the saved atlas and cited headings."""

import csv
import re
from collections import Counter
from functools import lru_cache
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
FIELDS = ('family', 'held', 'question', 'claim', 'direction', 'status',
          'evidence', 'frontier', 'origin')
QUESTIONS = ('criterion', 'failure', 'uniqueness')


@lru_cache(maxsize=None)
def anchors(path):
    counts = Counter()
    found = set()
    for heading in re.findall(r'^#{1,6} +(.+)$', path.read_text(), re.MULTILINE):
        slug = re.sub(r'[^\w -]', '', heading.lower().strip()).replace(' ', '-')
        repeat = counts[slug]
        found.add(f'{slug}-{repeat}' if repeat else slug)
        counts[slug] += 1
    return found


def main():
    atlas_path = ROOT / 'dev/research/existence-atlas/atlas.csv'
    with atlas_path.open(newline='') as stream:
        keys = {
            (row['family'], row['held'], question)
            for row in csv.DictReader(stream)
            for question in QUESTIONS
            if f'{question}_claim' in row
        }

    seen = set()
    grades = Counter()
    with (HERE / 'reviewed.csv').open(newline='') as stream:
        reader = csv.DictReader(stream)
        assert tuple(reader.fieldnames) == FIELDS, reader.fieldnames
        for row in reader:
            key = row['family'], row['held'] or 'all free', row['question']
            assert key in keys, f'Unknown atlas key {key}'
            assert key not in seen, f'Duplicate atlas key {key}'
            assert None not in row and all(row[field] is not None for field in FIELDS), key
            path, separator, anchor = row['evidence'].partition('#')
            assert separator and anchor in anchors(ROOT / path), (key, row['evidence'])
            seen.add(key)
            grades[row['status'], row['direction']] += 1
    assert seen, 'No reviewed rows'
    print(f'{len(seen)} unique atlas keys and evidence anchors verified')
    for (status, direction), count in sorted(grades.items()):
        print(f'{status}/{direction} {count}')


if __name__ == '__main__':
    main()
