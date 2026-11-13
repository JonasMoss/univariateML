"""Summarize retained numeric outputs, never fit or regenerate a truth table."""
import csv,hashlib,json
from pathlib import Path
root=Path(__file__).parent
summary={}
for name in ['roundtrip-before','roundtrip-after']:
 p=root/(name+'.csv')
 if name=='roundtrip-after' and (root/'roundtrip-final.csv').exists(): p=root/'roundtrip-final.csv'
 if not p.exists():continue
 rows=[]
 with p.open() as f:
  for r in csv.reader(f):
   id,lower,log,cases,up,down,invalid=r
   rows.append(dict(family=id,lower=bool(int(lower)),log=bool(int(log)),cases=int(cases),up=int(up),down=int(down),invalid=int(invalid)))
 summary[name]=rows
summary['samplers']={name:dict(rows=sum(1 for _ in (root/(name+'.csv')).open()),sha256=hashlib.sha256((root/(name+'.csv')).read_bytes()).hexdigest()) for name in ['samplers-before','samplers-after']}
summary['comparison']={}
with (root/'classified.csv').open() as f:
 for r in csv.DictReader(f):
  d=summary['comparison'].setdefault(r['family'],dict(disagreements=0,cpp_exact=0,stats_exact=0,adjusted_boundary_rounding=0,max_boundary_error_eps=0))
  d['disagreements']+=1
  d['cpp_exact']+=r['cpp']==r['exact']
  d['stats_exact']+=r['stats']==r['exact']
  if r['boundary_error_eps']:
   d['adjusted_boundary_rounding']+=1
   d['max_boundary_error_eps']=max(d['max_boundary_error_eps'],float(r['boundary_error_eps']))
(root/'summary.json').write_text(json.dumps(summary,indent=2)+'\n')
print(json.dumps(summary['comparison'],indent=2))
