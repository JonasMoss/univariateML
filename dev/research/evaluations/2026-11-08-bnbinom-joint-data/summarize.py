"""Summarize three paired workstation timings and check unchanged routes."""
import csv,json,statistics,hashlib,subprocess
from pathlib import Path
out=Path(__file__).parent
before=[list(csv.reader((out/f'before-{i}.csv').open())) for i in (1,2,3)]
after=[list(csv.reader((out/f'after-{i}.csv').open())) for i in (1,2,3)]
records=[]
for mask in range(8):
    b=[float(rows[mask][-1]) for rows in before]
    a=[float(rows[mask][-1]) for rows in after]
    assert all(rows[mask][1]=='0' for rows in before+after)
    if mask not in (1,4):
        assert all(rows[mask][1:6]==before[0][mask][1:6] for rows in before+after),mask
    for rows in before+after:
        for j in (2,3,4):
            assert abs(float(rows[mask][j])/float(before[0][mask][j])-1)<2e-6,(mask,j)
        assert abs(float(rows[mask][5])+1275.6474863695587)<1e-8
    if mask in (1,4):
        expected={1:[1.0672234787479615,2.1419791842743957,5.364733947436407],4:[1.0672234846760258,2.1419791623297685,5.3647338351087397]}[mask]
        for rows in after:
            assert all(abs(float(rows[mask][j+2])/value-1)<1e-8 for j,value in enumerate(expected)),mask
    records.append(dict(held_mask=mask,before_ms=statistics.median(b),after_ms=statistics.median(a),before_range=[min(b),max(b)],after_range=[min(a),max(a)]))
(out/'timings.json').write_text(json.dumps(dict(machine='workstation',repeats_per_timing=5,paired_runs=3,records=records),indent=2)+'\n')
paths=['univariateML-cpp/include/cppunivariate/distributions/bnbinom/'+n for n in ('mle.hpp','pair_certificate.hpp','README.md','test.cpp','golden.json','dpqr.hpp','truth.py','truth.json')]+['bindings/common/distributions/bnbinom/fit.hpp','bindings/r/distributions/bnbinom/test.R','bindings/js/test.mjs']
(out/'source.json').write_text(json.dumps(dict(base=subprocess.check_output(['git','rev-parse','HEAD'],text=True).strip(),sha256={p:hashlib.sha256(Path(p).read_bytes()).hexdigest() for p in paths}),indent=2)+'\n')
print(json.dumps(records,indent=2))
