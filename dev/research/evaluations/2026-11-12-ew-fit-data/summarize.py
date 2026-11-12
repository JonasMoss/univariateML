import csv,collections,json,statistics,sys
from pathlib import Path
p=Path(__file__).parent
source=sys.argv[1] if len(sys.argv)>1 else 'held-study.csv'
print(source)
rows=list(csv.DictReader((p/source).open()))
for mask in range(8):
 r=[x for x in rows if int(x['mask'])==mask]
 print(mask,dict(collections.Counter(x['status']+(' certified' if x['certified']=='1' else ' uncertified') for x in r)), 'median/max ms',round(statistics.median(float(x['ms']) for x in r),3),round(max(float(x['ms']) for x in r),3))
print('max absolute seam error',max(abs(float(x['seam_error'])) for x in rows if x['status']=='Ok'))
