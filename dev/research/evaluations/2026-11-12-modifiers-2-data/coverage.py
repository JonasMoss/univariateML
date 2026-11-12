"""Expose every hold subset and observed legacy outcome in the replay corpus."""
from pathlib import Path
from load_evidence import read_json
import collections,csv,json,yaml
root=Path.cwd();dest=Path(__file__).resolve().parent
cases=read_json(dest/'cases.json');rows=read_json(dest/'results.json')
families=sorted(set(r['family'] for r in rows));holds=[];outcomes=collections.Counter()
for family in families:
    text=(root/'univariateML-cpp/include/cppunivariate/distributions'/family/'README.md').read_text()
    names=[p['name'] for p in yaml.safe_load(text.split('---',2)[1])['params']]
    groups=collections.defaultdict(list);golden=set()
    for c,r in zip(cases,rows):
        if r['family']!=family:continue
        args=c['args'];held=tuple(p for p in names if p in args and args[p] is not None)
        groups[held].append(r)
        if c['source'].startswith('golden-'):golden.add(held)
        outcomes[(family,r['old_status'],r['old_kind'],r['old_certified'],r['old_message'])]+=1
    assert len(golden)==2**len(names),(family,len(golden),names)
    for held,group in sorted(groups.items()):
        holds.append(dict(family=family,held=';'.join(held),cases=len(group),Ok=sum(r['old_status']=='Ok' for r in group),stop=sum(r['old_status']=='stop' for r in group),golden_covered=held in golden))
with (dest/'hold-coverage.csv').open('w') as out:
    w=csv.DictWriter(out,fieldnames=list(holds[0]));w.writeheader();w.writerows(holds)
with (dest/'outcome-coverage.csv').open('w') as out:
    w=csv.writer(out);w.writerow(['family','status','stop_kind','certified','message','cases'])
    for key,count in outcomes.items():w.writerow([*key,count])
summary=dict(families=len(families),hold_subsets=len(holds),golden_hold_subsets=sum(h['golden_covered'] for h in holds),outcome_groups=len(outcomes),cases=len(rows))
(dest/'coverage-summary.json').write_text(json.dumps(summary,indent=2)+'\n')
print(summary)
