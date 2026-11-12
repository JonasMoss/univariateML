"""Exact-input closed-form maximum and adjacent-double representation check."""
import gzip,json,math
from pathlib import Path
import mpmath as mp
HERE=Path(__file__).resolve().parent
rows=[json.loads(line) for line in gzip.decompress((HERE/'final.jsonl.gz').read_bytes()).decode().splitlines()]
for row in rows:row['arithmetic']='extended'
double=HERE/'after-invgauss-double.jsonl'
has_double=double.exists() or double.with_suffix('.jsonl.gz').exists()
if has_double:
    text=double.read_text() if double.exists() else gzip.decompress(double.with_suffix('.jsonl.gz').read_bytes()).decode()
    extra=[json.loads(line) for line in text.splitlines()]
    for row in extra:row['arithmetic']='double'
    rows+=extra
out=[]
for row in rows:
    if row['family']!='invgauss' or row['index'] not in (3835,3838):continue
    checks=[]
    for digits in (100,160):
        mp.mp.dps=digits
        x=list(map(mp.mpf,row['data']));n=len(x);mu=mp.fsum(x)/n
        q=lambda m:mp.fsum((v-m)**2/(m*m*v) for v in x)
        lam=n/q(mu)
        def ll(m,s):
            return n/2*mp.log(s/(2*mp.pi))-mp.mpf('1.5')*mp.fsum(mp.log(v) for v in x)-s*q(m)/2
        maximum=ll(mu,lam)
        public=ll(*map(mp.mpf,row['parameters']))
        center=float(mu)
        points=[]
        for m in (math.nextafter(center,0.),center,math.nextafter(center,math.inf)):
            l0=float(n/q(mp.mpf(m)))
            for s in (math.nextafter(l0,0.),l0,math.nextafter(l0,math.inf)):
                points.append(ll(mp.mpf(m),mp.mpf(s)))
        checks.append(dict(digits=digits,maximum=mp.nstr(maximum,85),
          continuous_mean=mp.nstr(mu,85),continuous_shape=mp.nstr(lam,85),
          continuous_shape_binary64=float(lam),
          mean_relative_error=float(abs(mp.mpf(row['parameters'][0])/mu-1)),
          shape_relative_error=float(abs(mp.mpf(row['parameters'][1])/lam-1)),
          public_point=mp.nstr(public,85),public_relative_loss=float((maximum-public)/max(1,abs(maximum))),
          reported_relative_error=float(abs(mp.mpf(row['reported'])-maximum)/max(1,abs(maximum))),
          best_adjacent_relative_loss=float((maximum-max(points))/max(1,abs(maximum)))))
    assert checks[0]['maximum']==checks[1]['maximum']
    out.append(dict(index=row['index'],arithmetic=row['arithmetic'],data=row['data'],parameters=row['parameters'],checks=checks))
assert len(out)==(4 if has_double else 2)
(HERE/'invgauss-maximized-value.json').write_text(json.dumps(out,indent=2)+'\n')
failures=[r for r in out if r['checks'][-1]['reported_relative_error']>1e-10]
assert not failures,json.dumps(failures,indent=2)
assert all(r['checks'][-1]['shape_relative_error']<=1e-10 for r in out if r['arithmetic']=='double')
