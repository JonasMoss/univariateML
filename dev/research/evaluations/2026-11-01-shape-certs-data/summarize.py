import csv, math, statistics
from collections import Counter, defaultdict
from pathlib import Path
base = Path(__file__).parent
rows = list(csv.DictReader((base/'probes.csv').open()))
for family in ('ht','dgamma','slash'):
    r=[x for x in rows if x['family']==family]
    ok=[x for x in r if x['status']=='0' and math.isfinite(float(x['probe_ll']))]
    short=[x for x in ok if float(x['probe_ll'])-float(x['fit_ll']) > 1e-12*max(1,abs(float(x['fit_ll'])))]
    worst=max((float(x['probe_ll'])-float(x['fit_ll']) for x in ok),default=float('nan'))
    print(family, 'samples',len(r),'statuses',dict(Counter(x['status'] for x in r)), 'compared',len(ok),'shortfalls',len(short),'worst_probe_minus_fit',worst)
    for x in short: print('SHORTFALL',x)
path=base/'golden-time.csv'
if path.exists():
    for family in ('ht','dgamma','slash'):
        r=[x for x in csv.DictReader(path.open()) if x['family']==family]
        if r: print('golden',family,'n',r[0]['n'],'statuses',dict(Counter(x['status'] for x in r)),'median_ms',statistics.median(float(x['ms']) for x in r))
