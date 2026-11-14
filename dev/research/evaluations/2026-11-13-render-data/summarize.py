#!/usr/bin/env python3
"""Inventory observed tie choices, unchanged nonunique estimates and raw statuses."""
import collections,gzip,json,re
from pathlib import Path
HERE=Path(__file__).resolve().parent
BASE=HERE.parent/'2026-11-13-fit-snapshot-data'
def load(path):
 with gzip.open(path,'rt') as f:
  return {(r['family'],r['source'],r['case']):r for r in map(json.loads,f)}
summary={}
for mode in ('native','double','routes-native','routes-double'):
 old=load(BASE/(mode+'.jsonl.gz'));new=load(HERE/(mode+'.jsonl.gz'))
 ties=[];nonunique=[]
 for key,row in new.items():
  r=row['result'];prior=old[key]['result'];o=r.get('outcome',{})
  if prior['kind']=='tie':
   models=[c['model'] for c in r['candidates']]
   ties.append(dict(family=key[0],source=key[1],case=key[2],
     core_status=r['core_status'],outcome=o,models=models,
     coefficients=list(r['estimates']),held=row['held']))
  if prior['kind']=='ok' and o.get('unique') is False:
   nonunique.append(dict(family=key[0],source=key[1],case=key[2],core_status=r['core_status']))
 summary[mode]=dict(ties=ties,existing_nonunique=nonunique,
  ties_per_family=dict(collections.Counter(r['family'] for r in ties)),
  existing_nonunique_per_family=dict(collections.Counter(r['family'] for r in nonunique)))
(HERE/'payload-inventory.json').write_text(json.dumps(summary,indent=1)+'\n')
root=HERE.parents[3]
producers=[]
for f in sorted((root/'bindings/common/distributions').glob('*/*.hpp')):
 if re.search(r'(?<![!=])=\s*PayloadKind::Comparison|[,?:]\s*PayloadKind::Comparison|return\s+PayloadKind::Comparison',f.read_text()):
  producers.append(str(f.relative_to(root)))
(HERE/'comparison-adapters.json').write_text(json.dumps(producers,indent=1)+'\n')
for mode,items in summary.items():
 print(mode,'ties:',items['ties_per_family'],'existing nonunique:',items['existing_nonunique_per_family'])
