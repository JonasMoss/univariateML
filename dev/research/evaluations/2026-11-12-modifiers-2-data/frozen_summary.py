"""Tabulate the complete data-free audit, accepting raw or compressed ledgers."""
from pathlib import Path
from load_evidence import read_json
import collections, csv, json
p = Path(__file__).resolve().parent
rows = read_json(p/'frozen-audit.json')['results']
changes = [r for r in rows if r['differences']]
s = read_json(p/'frozen-audit-summary.json')
s['unclassified'] = sum(not r['reason'] for r in changes)
s['categories'] = dict(collections.Counter(r['reason'] for r in changes))
s['families'] = {f: dict(cases=sum(r['family']==f for r in rows),
                       differences=sum(r['family']==f for r in changes))
                 for f in sorted(set(r['family'] for r in rows))}
(p/'frozen-audit-summary.json').write_text(json.dumps(s,indent=2)+'\n')
with (p/'frozen-difference-reasons.csv').open('w') as out:
    w = csv.DictWriter(out,fieldnames=list(changes[0]))
    w.writeheader(); w.writerows(changes)
assert s['unclassified']==0
print(s['cases'],'fixed-law cases;',len(changes),'explained differences')
