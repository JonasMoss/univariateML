"""Three alternating before/after runs; invoke under one CPU reservation."""
import os
import csv
import io
import json
from pathlib import Path
import statistics
import subprocess
import sys
p=Path(__file__).resolve().parent
runs={'before':[],'after':[]}
suffix='-default' if os.getenv('UMVL_BENCH_DEFAULT') else ''
for repetition in range(3):
    for name,binary in zip(['before','after'],sys.argv[1:]):
        raw=subprocess.check_output([binary]+(['--default'] if suffix else []),text=True)
        runs[name].append(list(csv.DictReader(io.StringIO(raw))))
        if repetition==2:(p/f'{name}{suffix}.csv').write_text(raw)
summary={'machine':os.getenv('UMVL_BENCH_MACHINE','workstation GCC 15.2.0'),'repetitions':3,'control':{'tol':1e-8 if suffix else 1e-10,'maxit':500},'samples':500,'rows':[]}
for mask in range(8):
    before=statistics.median(float(rows[mask]['seconds']) for rows in runs['before'])
    after=statistics.median(float(rows[mask]['seconds']) for rows in runs['after'])
    summary['rows'].append(dict(held_mask=mask,before_seconds=before,after_seconds=after))
(p/f'timing-summary{suffix}.json').write_text(json.dumps(summary,indent=2)+'\n')
print(json.dumps(summary,indent=2))
