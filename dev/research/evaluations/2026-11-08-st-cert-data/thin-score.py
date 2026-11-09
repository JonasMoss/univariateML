from pathlib import Path
import json
import mpmath as mp
mp.mp.dps=80
root=Path(__file__).resolve().parent
rows=[]
for line in (root/'thin-score.txt').read_text().splitlines():
 nu,a,z,lo,hi=map(mp.mpf,line.split())
 m=nu+1;h=a*z*mp.sqrt(m/(nu+z*z))
 if h*h<10*m: continue  # This check targets the Student thin-tail regime.
 # Normalizer cancels analytically in the pdf / thin CDF ratio.
 logbeta=mp.loggamma(m/2)+mp.loggamma(mp.mpf('.5'))-mp.loggamma((m+1)/2)
 logpdf=-mp.log(m)/2-logbeta-(m+1)/2*mp.log1p(h*h/m)
 tail=mp.betainc(m/2,mp.mpf('.5'),0,m/(m+h*h),regularized=True)/2
 ratio=mp.exp(logpdf-mp.log(tail))
 v=m*z/(nu+z*z)-ratio*a*nu*mp.sqrt(m)/(nu+z*z)**mp.mpf('1.5')
 resolved=bool(mp.isfinite(lo) and mp.isfinite(hi))
 pad=128*mp.mpf(2)**-52*(1+max(abs(lo),abs(hi))) if resolved else mp.nan
 violation=max(lo-v,v-hi,mp.mpf(0)) if resolved else mp.nan
 rows.append(dict(nu=float(nu),alpha=float(a),z=float(z),lo=str(lo),hi=str(hi),reference=str(v),margin=str(pad),violation=str(violation),resolved=resolved,enclosed=bool(violation<=pad) if resolved else None))
(root/'thin-score-checks.json').write_text(json.dumps(rows,indent=2)+'\n')
print(json.dumps(dict(points=len(rows),unresolved=sum(not r['resolved'] for r in rows),failures=sum(r['resolved'] and not r['enclosed'] for r in rows),worst_relative_violation=max(float(mp.mpf(r['violation'])/(1+abs(mp.mpf(r['reference'])))) for r in rows if r['resolved']))))

assert all(r["enclosed"] for r in rows if r["resolved"])
