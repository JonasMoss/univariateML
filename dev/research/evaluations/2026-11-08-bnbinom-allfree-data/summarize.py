import csv,json,statistics,os
from pathlib import Path
out=Path(__file__).parent
before=[list(csv.reader((out/f'before-{i}.csv').open())) for i in (1,2,3)]
after=[list(csv.reader((out/f'after-{i}.csv').open())) for i in (1,2,3)]
records=[]
for mask in range(8):
    b=[float(rows[mask][-1]) for rows in before]
    a=[float(rows[mask][-1]) for rows in after]
    if mask:
        assert all(rows[mask][1:6]==before[0][mask][1:6] for rows in before+after),mask
    else:
        truth=[1.0672234392758119184,2.1419792202550637215,5.3647342962537853096]
        for rows in after:
            assert rows[mask][1]=='0'
            for j,value in enumerate(truth):assert abs(float(rows[mask][j+2])/value-1)<1e-8
            for j in (2,3,4):assert abs(float(rows[mask][j])/float(before[0][mask][j])-1)<2e-6
            assert abs(float(rows[mask][5])-float(before[0][mask][5]))<1e-8
    assert before[0][mask][1]=='0'
    records.append(dict(held_mask=mask,before_ms=statistics.median(b),after_ms=statistics.median(a),before_range=[min(b),max(b)],after_range=[min(a),max(a)]))
(out/'timings.json').write_text(json.dumps(dict(machine=os.environ.get('UMVL_BENCHMARK_MACHINE','workstation'),repeats_per_timing=5,paired_runs=3,unchanged_outputs_held_subsets=True,allfree_coefficients_within_golden_tolerance=True,records=records),indent=2)+'\n')
print(json.dumps(records,indent=2))
