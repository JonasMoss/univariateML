"""Independent 60-digit conditional-score roots; run after conditional.cpp."""
import json
import pathlib
import mpmath as mp
mp.mp.dps = 60
root = pathlib.Path(__file__).parent
x = list(map(mp.mpf, ['-1.7','-.9','-.2','.15','.3','.8','1.6','3.2']))
n = len(x)
phi = lambda t: mp.exp(-t*t/2)/mp.sqrt(2*mp.pi)
Phi = lambda t: mp.erfc(-t/mp.sqrt(2))/2
records = []
for row in (root/'conditional.txt').read_text().splitlines():
    aa,mm,ee,hh,ll,ok,gap,er,hr = row.split()
    a,e,h = map(mp.mpf, (aa,ee,hh)); m = int(mm)
    def scores(eta,b):
        g = [-(eta*v-b)+a*phi(a*(eta*v-b))/Phi(a*(eta*v-b)) for v in x]
        return n/eta+sum(v*t for v,t in zip(x,g)), -sum(g)
    if m == 0:
        es,hs = mp.findroot(scores,(e,h))
    elif m == 1:
        es = mp.findroot(lambda z: scores(z,mp.mpf('.2')*z)[0]+mp.mpf('.2')*scores(z,mp.mpf('.2')*z)[1], e)
        hs = mp.mpf('.2')*es
    else:
        es = 1/mp.mpf('1.3')
        hs = mp.findroot(lambda b: scores(es,b)[1],h)
    objective = lambda z,b: n*mp.log(z)+sum(mp.log(2*phi(z*v-b)*Phi(a*(z*v-b))) for v in x)
    errors = [abs(e-es)-mp.mpf(er), abs(h-hs)-mp.mpf(hr), objective(es,hs)-mp.mpf(ll)-mp.mpf(gap)]
    # Computed certificates are conditional on floating-point scores. Allow
    # 2e-12 for the separately measured double-evaluation roundoff here.
    assert ok == '1' and max(errors) < mp.mpf('2e-12'), (row, errors)
    records.append(dict(alpha=float(a),mask=m,eta_error=float(abs(e-es)),h_error=float(abs(h-hs)),likelihood_loss=float(objective(es,hs)-objective(e,h)),reported_gap=float(gap),max_roundoff_excess=float(max(errors))))
(root/'conditional-check.json').write_text(json.dumps(dict(dps=60,cases=records),indent=2)+'\n')
print('PASS:',len(records),'60-digit conditional roots; maximum rounding excess',max(r['max_roundoff_excess'] for r in records))
