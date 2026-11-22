"""Tabulate the step 8d records and diff the pre-existing comparisons."""

from collections import Counter, defaultdict
import csv
import difflib
import gzip
import json
from pathlib import Path
import sys

FAMILIES = dict(
    pareto2='E/F', invpareto='E/F', betapr='E/F', burr='E/F', gpd='F',
    ht='E/F', skellam='F', slash='F', sgomp='F', dgamma='F', paralogis='E/F',
    invparalogis='E/F', foldnorm='F', fatigue='F', cauchy='F', trgamma='E/F',
    invtrgamma='E/F', huber='E/F',
)
ADDITIONS = {
    'pareto2': {'transfer-flag'},
    'invpareto': {'transfer-flag'},
    'betapr': {'unit-face', 'root', 'gain', 'global', 'concave', 'coefficient', 'face'},
    'dgamma': {'joint-cover'},
    'ht': {'face-cover'},
    'sgomp': {'rate-cover', 'rate-enclosure', 'shape-enclosure'},
    'trgamma': {'power-basin', 'power-cover', 'power-face-gap'},
    'invtrgamma': {'power-basin', 'power-cover', 'power-face-gap'},
    'huber': {'epsilon-cover', 'epsilon-face'},
}


def mechanism(row, name):
    family, mask = row['family'], row['mask']
    if name in {'support', 'face-score', 'multiplicity', 'location', 'quartic'}:
        return 'D'
    if name == 'constant':
        return 'A'
    if name in {'unique', 'tie'}:
        return 'H'
    if name in {'face', 'face-cover', 'gain', 'limit', 'mapping', 'transfer-flag',
                'unit-face', 'power-face-gap', 'epsilon-face', 'collapse'}:
        return 'E'
    if ((family, mask, name) in {('invpareto', 1, 'root'), ('pareto2', 5, 'root'),
                                 ('betapr', 3, 'root'), ('gpd', 5, 'root'),
                                 ('dgamma', 1, 'bracket'), ('slash', 1, 'scale-bracket')}
            or family in {'paralogis', 'invparalogis'} and mask == 1 and name == 'scale-root'):
        return 'C'
    return 'F'


def read(path):
    with open(path) as stream:
        return [json.loads(line) for line in stream]


def comparable(rows, extensions=False):
    """Preserve the full comparison payload and record order, removing only time."""
    for row in rows:
        names = ADDITIONS.get(row['family'], ()) if extensions else ()
        comparisons = [c for c in row['comparisons'] if c['name'] not in names]
        if comparisons:
            record = dict(row, comparisons=comparisons)
            record.pop('seconds')
            yield json.dumps(record, sort_keys=True)+'\n'


def write_table(path, header, rows):
    with path.open('w', newline='') as stream:
        writer = csv.writer(stream, delimiter='\t')
        writer.writerow(header)
        writer.writerows(rows)


def main():
    baseline, final, output = sys.argv[1:]
    output = Path(output)
    before, after = read(baseline), read(final)
    master_records = list(comparable(before))
    old_records = list(comparable(after, extensions=True))
    delta = list(difflib.unified_diff(master_records, old_records, fromfile='master', tofile='replay-covers'))
    (output/'master-records.diff').write_text(''.join(delta))
    print(f'Master {len(master_records)} records, final pre-existing {len(old_records)} records, diff {len(delta)} lines')

    selected = [row for row in after if row['family'] in FAMILIES]
    selected_data = ''.join(json.dumps(row)+'\n' for row in selected).encode()
    (output/'records.jsonl.gz').write_bytes(gzip.compress(selected_data, mtime=0))
    verdicts = defaultdict(Counter)
    comparison_counts = Counter()
    mechanism_counts = defaultdict(Counter)
    mechanism_cases = Counter()
    cases = Counter()
    times = Counter()
    findings = defaultdict(list)
    for row in selected:
        family = row['family']
        cases[family] += 1
        times[family] += row['seconds']
        seen = set()
        for comp in row['comparisons']:
            name, verdict = comp['name'], comp['verdict']
            verdicts[family][verdict] += 1
            kind = mechanism(row, name)
            seen.add(kind)
            mechanism_counts[family, kind][verdict] += 1
            comparison_counts[family, kind, name, comp['rigor'], verdict] += 1
            if verdict in ('open', 'fails'):
                findings[verdict].append((row['mode'], family, row['source'], row['case'], row['mask'],
                                          name, comp['claim'], comp['hp'], comp['double'],
                                          comp['margin'], comp['note']))
        mechanism_cases.update((family, kind) for kind in seen)
    write_table(output/'families.tsv',
                ('family', 'mechanism', 'records', 'holds', 'open', 'fails', 'informational', 'record CPU s'),
                ((f, m, cases[f], verdicts[f]['holds'], verdicts[f]['open'], verdicts[f]['fails'],
                  verdicts[f]['info'], f'{times[f]:.3f}') for f, m in FAMILIES.items()))
    write_table(output/'mechanisms.tsv', ('family', 'mechanism', 'records', 'holds', 'open', 'fails', 'info'),
                ((*key, mechanism_cases[key], verdict['holds'], verdict['open'], verdict['fails'], verdict['info'])
                 for key, verdict in sorted(mechanism_counts.items())))
    write_table(output/'comparisons.tsv', ('family', 'mechanism', 'comparison', 'rigor', 'verdict', 'count'),
                ((*key, count) for key, count in sorted(comparison_counts.items())))
    for verdict in ('open', 'fails'):
        write_table(output/(verdict+'.tsv'),
                    ('mode', 'family', 'source', 'case', 'mask', 'comparison', 'claim', 'hp', 'double', 'margin', 'note'),
                    sorted(findings[verdict]))
    return bool(delta)


if __name__ == '__main__':
    sys.exit(main())
