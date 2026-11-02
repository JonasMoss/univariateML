import collections, csv, math, statistics
from pathlib import Path
p = Path(__file__).with_name('comparison.csv')
rows = list(csv.DictReader(p.open()))
seeded, timing = rows[:600], rows[600:]
transitions = collections.Counter((r['old_status'], r['new_status']) for r in seeded)
print('seeded',len(seeded),'transitions',sorted(transitions.items()))
paired=[r for r in seeded if r['old_status']=='0' and r['new_status']=='0']
deltas=[float(r['new_ll'])-float(r['old_ll']) for r in paired]
print('paired',len(paired),'min_delta',min(deltas,default=math.nan),'max_delta',max(deltas,default=math.nan))
print('losses > 1e-8 n',sum(d < -1e-8*int(r['n']) for d,r in zip(deltas,paired)))
for r in seeded:
    if r['old_status'] != r['new_status'] or (r['old_status']=='0' and r['new_status']=='0' and abs(float(r['new_ll'])-float(r['old_ll']))>1e-8*int(r['n'])):
        print('difference',','.join(r.values()))
print('timing paired',len(timing),'old median ms',statistics.median(float(r['old_ms']) for r in timing),'new median ms',statistics.median(float(r['new_ms']) for r in timing))
print('timing statuses',collections.Counter((r['old_status'],r['new_status']) for r in timing))
