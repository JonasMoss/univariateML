import pathlib,json,mpmath as mp
mp.mp.dps=80
p=pathlib.Path(__file__).resolve().parent
rows=[]
for line in (p/'moment-probe.txt').read_text().splitlines():
    nu,m=line.split();nu=mp.mpf(nu);actual=mp.mpf(m)
    expected=mp.exp(mp.loggamma((nu-1)/2)-mp.loggamma(nu/2))*mp.sqrt((nu-2)/mp.pi)
    rows.append(dict(nu=str(nu),computed=str(actual),reference=str(expected),absolute_error=float(abs(actual-expected))))
(p/'moment-checks.json').write_text(json.dumps(rows,indent=2)+'\n')
for row in rows:print(row['nu'],row['computed'],row['absolute_error'])
assert max(r['absolute_error'] for r in rows if mp.mpf(r['nu'])>=128)>1e-5
