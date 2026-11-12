"""Collect retained census/stress results; run with /tmp/umvl-cpu/run."""
import csv,json,collections,statistics,hashlib
from pathlib import Path

p=Path(__file__).resolve().parent
out={}
for name in ['held-study','held-study-budget1000','held-study-double','archive','archive-double']:
    rows=list(csv.DictReader((p/(name+'.csv')).open()))
    groups=[]
    for mask in range(8):
        group=[r for r in rows if int(r['mask'])==mask]
        times=[float(r['ms']) for r in group]
        groups.append(dict(mask=mask,statuses=dict(collections.Counter(r['status']+(' certified' if r['certified']=='1' else ' uncertified') for r in group)),median_ms=statistics.median(times),max_ms=max(times)))
    out[name]=dict(fits=len(rows),statuses=dict(collections.Counter(r['status'] for r in rows)),holds=groups)
for name in ['reference-check','held-study-double-reference','archive-reference','archive-double-reference']:
    data=json.loads((p/(name+'.json')).read_text())
    records=data['records']
    out[name]=dict(roots=len(records),worst_relative_coefficient_error=max(r['relative_coefficient_error'] for r in records),worst_relative_or_absolute_logLik_error=max(r['logLik_relative_error'] for r in records))
out['frozen_sample_sha256']={f:hashlib.sha256((p/f).read_bytes()).hexdigest() for f in ['held-samples.csv','archive-samples.csv']}
(p/'summary.json').write_text(json.dumps(out,indent=2)+'\n')
