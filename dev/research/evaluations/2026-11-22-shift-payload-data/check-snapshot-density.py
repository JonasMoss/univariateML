import collections
import gzip
import json
from pathlib import Path
import runpy
import sys

scope = Path(sys.argv[1])
target = Path(sys.argv[2])
validation = Path('dev/validation').resolve()
sys.path.insert(0, str(validation))
density = runpy.run_path(str(validation/'fit-density.py'))
fs = density['fs']
archive = collections.defaultdict(list)
with gzip.open(fs.ARCHIVE, 'rt') as source:
    for case in json.load(source):
        archive[case['family']].append(case)
table = density['laws']()
shifted = density['SHIFTED_LAWS']
failures = {}
with target.open('w') as records:
    for mode in ('native', 'double'):
        rows = density['load'](f'{scope/mode},{scope/("routes-"+mode)}')
        applicable = {}
        for key, row in rows.items():
            result = row['result']
            if result.get('model') in shifted or any(c['model'] in shifted for c in result.get('candidates', ())):
                applicable[key] = row
        errors, judged, stats, unchecked = density['evaluate'](applicable, mode, table, archive, records, False)
        failures.update(errors)
        print(mode, 'payload rows', len(applicable), 'judged', len(judged),
              'evaluated by family', {key: len(v) for key, v in sorted(stats.items())},
              'unchecked', dict(unchecked))
for key, error in sorted(failures.items()):
    print(key, error)
sys.exit(bool(failures))
