"""Locate the largest accepted error separately for density, CDF and quantile."""
import json
import check
rows = check.jobs()
results = {}
for mode in ['native', 'double']:
    worst = {}
    for r, value in zip(rows, check.study.evaluate([r['job'] for r in rows], mode)):
        j = r['job']; actual = float(value)
        error = check.error(actual, r['expected'], j[1]==2 or (j[1]==1 and not j[4]))
        key = j[0]+':'+['d','p','q'][j[1]]
        if key not in worst or error>worst[key]['error']:
            worst[key] = dict(job=j, expected=r['expected'], actual=actual, error=error)
        assert error <= (1e-9 if j[1]==2 else 1e-10)
    results[mode] = worst
(check.H/'error-breakdown.json').write_text(json.dumps(results, indent=2)+'\n')
for mode, worst in results.items():
    print(mode, {kind:max(v['error'] for k,v in worst.items() if k.endswith(':'+kind)) for kind in ['d','p','q']})
