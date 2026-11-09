import csv
import json
import statistics
from collections import Counter
from pathlib import Path

out = Path(__file__).parent
rows = list(csv.DictReader((out / 'coverage.csv').open()))
assert len(rows) == 270 and {int(r['id']) for r in rows} == set(range(270))
samples = [json.loads(s) for s in (out / 'coverage-samples.jsonl').read_text().splitlines()]
assert len(samples) == 270 and {s['id'] for s in samples} == set(range(270))
categories = ('certified_finite', 'proved_face', 'uncertified_legacy', 'search_failure')

def summarize(selected):
    counts = Counter(r['category'] for r in selected)
    assert set(counts) <= set(categories)
    times = [float(r['fit_ms']) for r in selected]
    return dict(samples=len(selected), counts={k: counts[k] for k in categories},
                fractions={k: counts[k]/len(selected) for k in categories},
                mean_ms=statistics.mean(times), median_ms=statistics.median(times),
                maximum_ms=max(times), total_seconds=sum(times)/1000)

result = dict(design='fixed grid, two seeds per cell; descriptive frequencies',
              tol=1e-8, iterlim=600,
              machines=sorted({r['machine'] for r in rows}),
              overall=summarize(rows),
              by_source={s: summarize([r for r in rows if r['source']==s])
                         for s in ('bnbinom','nbinom','pois')},
              by_source_n={f'{s}-n{n}': summarize([r for r in rows if r['source']==s and int(r['n'])==n])
                           for s in ('bnbinom','nbinom','pois') for n in (30,200,1000)},
              uncertified_by_status=dict(Counter(r['status'] for r in rows if r['category']=='uncertified_legacy')),
              certified_finite_with_inner_profile=sum(r['category']=='certified_finite' and r['profile_closed']=='1' for r in rows),
              failure_ids=[int(r['id']) for r in rows if r['category']=='search_failure'])
for r in rows:
    assert int(r['seed']) == 11654000+int(r['id'])
    assert int(r['certified']) == (r['category'] in ('certified_finite','proved_face'))
    assert len(samples[int(r['id'])]['x']) == int(r['n'])
(out / 'coverage.json').write_text(json.dumps(result, indent=2)+'\n')
print(json.dumps(result, indent=2))
