"""Validate the report's coverage, anchors and proposed atlas rows."""

import csv
import re
from pathlib import Path

DATA = Path(__file__).resolve().parent
EVALUATIONS = DATA.parent
REPORT = EVALUATIONS / '2026-11-27-uniq-atlas-math-report.md'
ROOT = EVALUATIONS.parents[2]


def read_csv(path):
    with path.open(newline='') as handle:
        return list(csv.DictReader(handle))


def slug(heading):
    return re.sub(r'[^\w -]', '', heading.lower()).replace(' ', '-')


def anchors(path):
    return {
        slug(heading) for heading in re.findall(r'^#{1,6} +(.+)$', path.read_text(), re.M)
    }


def main():
    atlas = {
        (r['family'], '' if r['held'] == 'all free' else r['held'])
        for r in read_csv(EVALUATIONS.parent / 'existence-atlas/atlas.csv')
    }
    reference = read_csv(EVALUATIONS.parent / 'existence-atlas/proofs.csv')
    proofs = read_csv(DATA / 'proofs.csv')
    assert list(proofs[0]) == list(reference[0])
    text = REPORT.read_text()
    report_anchors = anchors(REPORT)
    summary = {}
    for family, held, question, result in re.findall(
        r'^\| ([a-z0-9]+) \| ([a-zA-Z0-9+ ]+) \| (criterion|failure|uniqueness) \| (proved|refuted|partial|open) \|$',
        text, re.M,
    ):
        held = '' if held == 'all free' else held
        key = family, held, question
        assert key not in summary, key
        summary[key] = result
        assert slug(f'{family} held {held or "all free"}') in report_anchors, key

    assert len(summary) == 40
    assert len({(f, h) for f, h, q in summary}) == 31
    assert len({f for f, h, q in summary}) == 14
    expected = {key for key, outcome in summary.items() if outcome != 'open'}
    found = set()
    for row in proofs:
        family, held, question = (row[k] for k in ('family', 'held', 'question'))
        key = family, held, question
        assert key in expected and key not in found and (family, held) in atlas, key
        found.add(key)
        assert row['claim'] and row['status'] == 'written' and row['origin'], key
        if summary[key] == 'partial':
            assert row['direction'] == 'one-sided' and row['frontier'], key
        else:
            assert row['direction'] == ('iff' if question == 'criterion' else 'exact'), key
        evidence_path, fragment = row['evidence'].split('#')
        assert (ROOT / evidence_path).resolve() == REPORT, key
        assert fragment == slug(f'{family} held {held or "all free"}') and fragment in report_anchors, key
    assert found == expected, (found - expected, expected - found)

    links = re.findall(r'\]\(([^)\s]+)\)', text)
    for link in links:
        if '://' in link:
            continue
        target, _, fragment = link.partition('#')
        path = (REPORT.parent / target).resolve() if target else REPORT
        assert path.is_file(), link
        if fragment and path.suffix == '.md':
            assert fragment in anchors(path), link
        if fragment and path.suffix == '.tex':
            assert f'\\label{{{fragment}}}' in path.read_text(), link
    print(f'{len(summary)} questions, {len(proofs)} proof rows and {len(links)} report links checked')


if __name__ == '__main__':
    main()
