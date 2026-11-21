#!/usr/bin/env python3
"""Overlay a result-payload marker on just one family's records in two modes."""
import argparse
import json
from pathlib import Path
import shutil

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--family', default='lomax')
args = parser.parse_args()
for mode in ('native', 'double'):
    orig = Path('build/snapshots')/mode
    dest = Path('build/scratch')/('touch-'+mode)
    dest.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(orig/'provenance.json', dest/'provenance.json')
    filename = args.family+'.jsonl'
    header, *rows = (orig/filename).read_text().splitlines()
    with (dest/filename).open('w') as out:
        out.write(header+'\n')
        for text in rows:
            row = json.loads(text)
            row['result']['_strict_par_probe'] = True
            out.write(json.dumps(row, sort_keys=True)+'\n')
    print(mode, args.family, len(rows), 'records touched')
