import json, pathlib, statistics
p=pathlib.Path(__file__).resolve().parent
runs={stage:[[line.split() for line in (p/f'timing-{stage}-{i}.txt').read_text().splitlines()] for i in range(1,4)] for stage in ['before','after']}
for stage in runs:
    assert all(len(run)==16 for run in runs[stage])
for before,after in zip(runs['before'],runs['after']):
    for a,b in zip(before,after):
        if a[0]!='7':assert a[2:]==b[2:],(a,b)
rows=[]
for mask in range(16):
    after=runs['after'][0][mask]
    rows.append(dict(mask=mask,held=[name for i,name in enumerate(['mean','sd','nu','xi']) if mask>>i&1],before_ms=statistics.median(float(r[mask][1])*1000 for r in runs['before']),after_ms=statistics.median(float(r[mask][1])*1000 for r in runs['after']),status=int(after[2]),certified=bool(int(after[3])),coefficients=[float(v) for v in after[4:8]],logLik=float(after[8])))
(p/'timings.json').write_text(json.dumps(dict(sample='frozen sstd 500 observations',held_values={'mean':0,'sd':1.4,'nu':5,'xi':1.4},repetitions=3,unchanged_routes_equal_in_all_repetitions=15,rows=rows),indent=2)+'\n')
for r in rows:print(r['mask'],','.join(r['held']) or 'none',f"{r['before_ms']:.3f}",f"{r['after_ms']:.3f}",r['certified'])
