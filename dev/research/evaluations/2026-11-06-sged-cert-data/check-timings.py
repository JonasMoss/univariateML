"""Check retained paired benchmark coefficients without regenerating goldens."""
import json
from pathlib import Path
root=Path(__file__).resolve().parent
before=[list(map(float,s.split())) for s in (root/'timings-before.txt').read_text().splitlines()]
after=[list(map(float,s.split())) for s in (root/'timings-after.txt').read_text().splitlines()]
golden=json.loads((root.parents[3]/'univariateML-cpp/include/cppunivariate/distributions/sged/golden.json').read_text())
assert len(before)==len(after)==5
rows=[]
for a,b in zip(before,after):
    assert a[0]==b[0] and a[2]==b[2]==0
    error=max(abs(v-w)/(1+abs(v)) for v,w in zip(a[3:7],b[3:7]))
    assert error<1.220703e-4
    rows.append(dict(case=int(a[0]),before_wall=a[1],after_wall=b[1],before_cpu=a[8],after_cpu=b[8],max_coefficient_delta=error,loglik_delta=b[7]-a[7]))
expected=[golden['estimates'][k] for k in ['mean','sd','nu','xi']]+[golden['logLik']]
assert before[0][3:8]==after[0][3:8]==expected
summary=dict(default_exact_golden=True,rows=rows)
(root/'timing-results.json').write_text(json.dumps(summary,indent=2)+'\n')
print(json.dumps(summary,indent=2))
