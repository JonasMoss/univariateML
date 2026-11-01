import csv, math, statistics
from pathlib import Path
p=Path(__file__).parent
rows=list(csv.DictReader((p/'paired.csv').open()))
assert len(rows)==2400
changes=losses=differences=0
for r in rows:
    changes+=r['old_status']!=r['new_status']
    a,b=float(r['old_ll']),float(r['new_ll'])
    if math.isfinite(a):
        losses+=a-b>1e-12*max(1,abs(a))
        differences+=any(r['old_'+v]!=r['new_'+v] for v in ['ll','mu','sigma','it'])
assert changes==losses==differences==0
out=[f'600 samples, 2400 pairs; status changes {changes}; likelihood losses {losses}; differing finite results {differences}.', 'n mask old_mean_s new_mean_s speedup']
for n in [8,32,128,500]:
    for mask in range(4):
        sub=[r for r in rows if int(r['n'])==n and int(r['mask'])==mask]
        a=statistics.mean(float(r['old_s']) for r in sub);b=statistics.mean(float(r['new_s']) for r in sub)
        out.append(f'{n} {mask} {a:.9f} {b:.9f} {a/b:.3f}')
out.append('golden mask old_median_s new_median_s speedup')
g=list(csv.DictReader((p/'golden.csv').open()))
for mask in range(4):
    sub=[r for r in g if int(r['mask'])==mask]
    assert all(r['old_ll']==r['new_ll'] and r['old_mu']==r['new_mu'] and r['old_sigma']==r['new_sigma'] for r in sub)
    a=statistics.median(float(r['old_s']) for r in sub);b=statistics.median(float(r['new_s']) for r in sub)
    out.append(f'{mask} {a:.9f} {b:.9f} {a/b:.3f}')
text='\n'.join(out)+'\n'
(p/'summary.txt').write_text(text)
print(text)
