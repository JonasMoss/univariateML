"""Extract every value difference above 1e-12 from the convention probes."""
import csv,json,math
from pathlib import Path
root=Path(__file__).resolve().parent
rows=[]
for case,pair in json.loads((root/'conventions.json').read_text()).items():
    for i,(a,b) in enumerate(zip(pair['core']['value'],pair['stats']['value'])):
        try:
            x,y=float(a),float(b)
            if math.isnan(x) and math.isnan(y) or x==y: continue
            relative=abs(x-y)/(abs(y) if y else 1) if math.isfinite(x) and math.isfinite(y) else math.inf
        except (ValueError,TypeError):
            if a==b: continue
            relative=math.inf
        if not math.isfinite(relative) or relative>1e-12:
            rows.append(dict(case=case,index=i+1,core=a,stats=b,relative=relative))
with (root/'convention-changes.csv').open('w') as stream:
    writer=csv.DictWriter(stream,fieldnames=['case','index','core','stats','relative'])
    writer.writeheader();writer.writerows(rows)
print(len(rows),'material convention differences')
