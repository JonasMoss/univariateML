"""Replay the original TASK-196 measurements without regenerating references."""
import json
import check

H = check.H
source = check.study.ROOT / 'dev/research/evaluations/2026-11-11-oracle-gaps-data'
rows = []
for exclusion in json.loads((source / 'row-exclusions.json').read_text()):
    if exclusion['family'] in check.study.F:
        rows.extend(exclusion['measurements'])
rows.extend(r for r in json.loads((source / 'open-point-final-probes.json').read_text())
            if r['family'] in check.study.F)
unique = {}
for r in rows:
    job = [r['family'], {'d': 0, 'p': 1, 'q': 2}[r['function']],
           r['input'], int(r['lower']), int(r['log']), r['parameters']]
    unique[json.dumps(job)] = dict(job=job, expected=float(r['expected']))
rows = list(unique.values())
points = {}
for r in rows:
    j = r['job']
    key = json.dumps([j[0], j[5]])
    point = points.setdefault(key, dict(family=j[0], parameters=j[5], rows=[], worst_error={}))
    point['rows'].append(dict(operation=j[1], input=j[2], lower=j[3], log=j[4], expected=r['expected']))
corpus = {json.dumps([j[0], j[1], j[2], int(j[3]), int(j[4]), j[5]])
          for r in check.jobs() for j in [r['job']]}
result = dict(checks_per_mode=len(rows),
              missing_from_main_corpus=[r for r in rows if json.dumps(r['job']) not in corpus],
              modes={})
for mode in ['native', 'double']:
    actual = check.study.evaluate([r['job'] for r in rows], mode)
    failures = []
    worst = 0
    for r, value in zip(rows, actual):
        j = r['job']
        error = check.error(float(value), r['expected'], j[1] == 2 or (j[1] == 1 and not j[4]))
        point = points[json.dumps([j[0], j[5]])]
        point['worst_error'][mode] = max(point['worst_error'].get(mode, 0), error)
        tol = 1e-9 if j[1] == 2 else 1e-10
        if error > tol:
            failures.append(dict(r, actual=float(value), error=error))
        else:
            worst = max(worst, error)
    result['modes'][mode] = dict(failures=failures, worst_pass=worst)
result['points'] = list(points.values())
(H / 'oracle-points.json').write_text(json.dumps(result, indent=2) + '\n')
print(json.dumps(result, indent=2))
assert not result['missing_from_main_corpus']
assert all(not r['failures'] for r in result['modes'].values())
