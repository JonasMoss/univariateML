"""Check proof schema, unique atlas keys, and report anchors."""
import csv
import re
from pathlib import Path


atlas = Path('dev/research/existence-atlas')
rows_path = Path(__file__).with_name('proofs.csv')
with (atlas / 'atlas.csv').open() as handle:
    regimes = {(row['family'], '' if row['held'] == 'all free' else row['held'])
               for row in csv.DictReader(handle)}
with (atlas / 'proofs.csv').open() as handle:
    fields = next(csv.reader(handle))
with rows_path.open() as handle:
    reader = csv.DictReader(handle)
    assert reader.fieldnames == fields
    rows = list(reader)

keys = [(row['family'], row['held'], row['question']) for row in rows]
assert len(keys) == len(set(keys)) == 37
assert {key[:2] for key in keys} <= regimes
for row in rows:
    assert row['status'] == 'written' and row['claim']
    assert row['question'] in {'criterion', 'failure', 'uniqueness'}
    assert row['direction'] in {'iff', 'exact', 'one-sided'}
    file, anchor = row['evidence'].split('#')
    headings = re.findall(r'^### (.*)$', Path(file).read_text(), re.M)
    assert anchor in {re.sub(r'[^\w -]', '', heading.lower()).replace(' ', '-')
                      for heading in headings}, row
print(f'Validated {len(rows)} proof rows and their report anchors.')
