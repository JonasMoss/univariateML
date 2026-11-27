"""Check staged atlas coverage, evidence links, and recorded interval margins."""
import csv
from decimal import Decimal
import json
from pathlib import Path
import re

data = Path(__file__).resolve().parent
atlas = data.parents[1] / 'existence-atlas'
root = data.parents[3]
report = data.with_name(data.name.removesuffix('-data') + '-report.md')


def read_csv(path):
    with path.open(newline='') as handle:
        reader = csv.DictReader(handle)
        return reader.fieldnames, list(reader)


def anchors(path):
    return {re.sub(r'[^\w\s-]', '', title.strip().lower()).replace(' ', '-')
            for title in re.findall(r'^#{1,6} (.+)$', path.read_text(), re.M)}


def atlas_check():
    header, rows = read_csv(data/'proofs.csv')
    assert header == read_csv(atlas/'proofs.csv')[0]
    masks = {
        ('std', mask) for mask in ['', 'mean', 'mean+sd', 'nu', 'nu+sd', 'sd']
    } | {('lst', mask) for mask in ['', 'df', 'df+sigma', 'mu', 'sigma']}
    atlas_masks = {(r['family'], r['held'].replace('all free', ''))
                   for r in read_csv(atlas/'atlas.csv')[1]}
    assert masks <= atlas_masks
    assert ('lst', 'mu+sigma') not in atlas_masks
    questions = {'criterion': 'iff', 'failure': 'exact', 'uniqueness': 'exact'}
    expected = {(family, held, q) for family, held in masks for q in questions}
    got = [(r['family'], r['held'], r['question']) for r in rows]
    assert len(got) == len(expected) and set(got) == expected
    report_anchors = anchors(report)
    for row in rows:
        assert None not in row and None not in row.values(), row
        assert row['direction'] == questions[row['question']], row
        assert row['status'] == 'written' and row['claim'], row
        path, anchor = row['evidence'].split('#', 1)
        mask = (row['held'] or 'all-free').replace('+', '')
        assert root/path == report and anchor == f"{row['family']}-held-{mask}"
        assert anchor in report_anchors, row
    return len(rows)


def link_check():
    links = re.findall(r'\[[^\n\]]*\]\(([^)\n]+)\)', report.read_text())
    checked = 0
    for target in links:
        if target.startswith('https://'):
            continue
        name, _, anchor = target.partition('#')
        path = report.parent/name if name else report
        assert path.exists(), target
        if anchor and path.suffix == '.md':
            assert anchor in anchors(path), target
        checked += 1
    return checked


def margin_check(name, bounds):
    result = json.loads((data/name).read_text())
    for key, value in bounds.items():
        count, gap = result[key]
        assert count > 0 and Decimal(gap) > Decimal(value), (key, gap)
        assert value in report.read_text(), value


if __name__ == '__main__':
    rows, links = atlas_check(), link_check()
    margin_check('crossing.json', {
        'path gap 158 vs 30': '.0000131',
        'path gap 5.5 vs 2': '.00610',
        'path gap 5.5 vs None': '.01452',
        'low-shape derivative': '.000419',
        'left endpoint excludes right': '.00000298',
        'right endpoint excludes left': '.00000129',
    })
    margin_check('scalar_crossing.json', {
        'path gap 135 vs 30': '.0000193',
        'path gap 8.25 vs None': '.01621',
        'left endpoint excludes right': '.00000120',
        'right endpoint excludes left': '.00000262',
    })
    print(f'{rows} atlas rows, {links} local links and ten interval margins passed.')
