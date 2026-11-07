"""80-digit stationarity oracle for seed-8104 last sample, p held at .7."""
import json
from pathlib import Path
import mpmath as m
m.mp.dps=80
root=Path(__file__).parent
x=[m.mpf(v) for v in json.loads((root/'hold-data.json').read_text())[-1]]
a=m.mpf(.7);n=len(x)
def scores(b,s):
    return n*(m.digamma(a+b)-m.digamma(b))-sum(m.log1p(v/s) for v in x), -n*a+(a+b)*sum(v/(s+v) for v in x)
b,s=m.findroot(scores,(m.mpf(95.5),m.mpf(78.2)),tol=m.mpf('1e-65'))
ll=(a-1)*sum(m.log(v) for v in x)-n*a*m.log(s)-(a+b)*sum(m.log1p(v/s) for v in x)-n*m.log(m.beta(a,b))
print(json.dumps(dict(precision=80,a=str(a),b=str(b),scale=str(s),logLik=str(ll),scores=[str(v)for v in scores(b,s)]),indent=2))
