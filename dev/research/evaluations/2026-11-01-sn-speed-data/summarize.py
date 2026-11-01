import csv, collections, statistics, math
from pathlib import Path
base=Path(__file__).resolve().parent
rows=list(csv.DictReader((base/'paired.csv').open()))
assert len(rows)==612*8, len(rows)
changes=[];losses=[];nonidentical=[]
for r in rows:
 if r['old_status']!=r['new_status']: changes.append(r)
 old,new=float(r['old_ll']),float(r['new_ll'])
 if math.isfinite(old) and (not math.isfinite(new) or old-new>1e-12*max(1,abs(old))): losses.append(r)
 if r['old_ll']!=r['new_ll'] or r['old_evaluations']!=r['new_evaluations']: nonidentical.append(r)
print('fits',len(rows),'samples',len(set(r['sample'] for r in rows)))
print('status changes',len(changes),'losses',len(losses),'nonidentical ll/evaluations',len(nonidentical))
print('statuses',dict(collections.Counter(r['new_status'] for r in rows)))
print('n,mask,old mean seconds,new mean seconds,speedup')
for n in (8,32,128,500):
 for mask in range(8):
  group=[r for r in rows if int(r['n'])==n and int(r['mask'])==mask]
  ot=statistics.mean(float(r['old_seconds']) for r in group);nt=statistics.mean(float(r['new_seconds']) for r in group)
  print(n,mask,ot,nt,ot/nt,sep=',')
print('golden mask,old median,new median,speedup')
gold=list(csv.DictReader((base/'golden.csv').open()))
for mask in range(8):
 group=[r for r in gold if int(r['mask'])==mask]
 ot=statistics.median(float(r['old_seconds']) for r in group);nt=statistics.median(float(r['new_seconds']) for r in group)
 print(mask,ot,nt,ot/nt,sep=',')
assert not changes and not losses and not nonidentical
