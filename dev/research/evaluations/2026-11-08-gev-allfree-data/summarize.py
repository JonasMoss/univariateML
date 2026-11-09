import pathlib,json,statistics,decimal
root=pathlib.Path(__file__).parent
before=[];after=[]
for j in range(1,4):
 before.append([line.split() for line in (root/f'golden-before-{j}.txt').read_text().splitlines()])
 after.append([line.split() for line in (root/f'golden-after-{j}.txt').read_text().splitlines()])
rows=[]
labels=['GEV default n=7','GEV shape-only n=7','GEV half tie n=4','Frechet default n=7','Frechet half tie n=4','GEV default n=500','Frechet default n=500','GEV shape-only n=500','GEV two-point n=10001']
for i,name in enumerate(labels):
 assert all(before[j][i][:6]==after[j][i][:6] for j in range(3)),(i,before[0][i],after[0][i])
 rows.append(dict(subset=name,before_us=statistics.median(float(b[i][6]) for b in before),after_us=statistics.median(float(b[i][6]) for b in after)))
for family,index in [('gev',5),('frechet',6)]:
 g=json.loads(pathlib.Path(f'univariateML-cpp/include/cppunivariate/distributions/{family}/golden.json').read_text())
 names=['mu','sigma','xi'] if family=='gev' else ['lambda','mu','sigma']
 for name,computed in zip(names,map(float,after[0][index][2:5])):
  expected=g['estimates'][name]
  assert abs(computed-expected)<=1e-10*abs(expected),(family,name,computed,expected)
 assert abs(float(after[0][index][5])-g['logLik'])<=1e-10*abs(g['logLik'])
two_before=[x.split() for x in (root/'before.txt').read_text().splitlines()]
two_after=[x.split() for x in (root/'after.txt').read_text().splitlines()]
for a,b in zip(two_before,two_after):
 rows.append(dict(subset=f'Frechet two-point n=20 k={a[0]}',before_us=float(a[6]),after_us=float(b[6]),before_status=int(a[1]),after_status=int(b[1])))
ref=json.loads((root/'independent.json').read_text())
r=next(v for v in ref['references'] if v['p']=='0.45')
computed=two_after[2]
assert abs(float(computed[3])+float(r['h']))<1e-12
assert abs(float(computed[4])-float(r['sigma']))<1e-12
assert abs(float(computed[5])-20*float(r['finite_loglik_per_n']))<1e-10
held_before=[line.split() for line in (root/'held-before.txt').read_text().splitlines()]
held_after=[line.split() for line in (root/'held-after.txt').read_text().splitlines()]
for name,a,b in zip(['GEV two-point held mu n=4','GEV two-point held sigma n=4','Frechet two-point held sigma n=16'],held_before,held_after):
 rows.append(dict(subset=name,before_us=float(a[6]),after_us=float(b[6])))
decimal.getcontext().prec=70
reference_mu=-(decimal.Decimal(63).sqrt()-7)/16
assert abs(float(held_after[2][3])-float(reference_mu))<1e-14
assert float(held_after[2][4])==.125
print(json.dumps(dict(defaults_bit_identical=True,goldens_relative_tolerance=1e-10,held_frechet_exact_mu='-(sqrt(63)-7)/16',held_frechet_reference_mu=str(reference_mu),timings=rows),indent=2))
