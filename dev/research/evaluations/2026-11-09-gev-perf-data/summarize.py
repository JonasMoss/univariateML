import pathlib,json,statistics,math
p=pathlib.Path(__file__).resolve().parent
before=[json.loads(l) for l in (p/'baseline.jsonl').read_text().splitlines()]
after=[json.loads(l) for l in (p/'final.jsonl').read_text().splitlines()]
rows=[]
for b,a in zip(before,after):
 assert (a['case'],a['n'])==(b['case'],b['n'])
 assert a['status']==b['status']==0 and a['budget']==b['budget']==False
 for x,y in zip(a['coef'],b['coef']):assert abs(x-y)<=1e-10*max(abs(y),1e-5),(a,b)
 if a['n']==500:
  g=json.loads(pathlib.Path(f"univariateML-cpp/include/cppunivariate/distributions/{a['case']}/golden.json").read_text())
  names=['mu','sigma','xi'] if a['case']=='gev' else ['lambda','mu','sigma']
  for name,value in zip(names,a['coef']):assert abs(value-g['estimates'][name])<=1e-10*abs(g['estimates'][name]),(a,name)
  assert abs(a['logLik']-g['logLik'])<=1e-10*abs(g['logLik'])
 rows.append(dict(case=a['case'],n=a['n'],before_seconds=b['microseconds']/1e6,after_seconds=a['microseconds']/1e6,speedup=b['microseconds']/a['microseconds'],before_work=b['evaluations_or_iterations'],after_work=a['evaluations_or_iterations']))
# Positions are explicit to detect accidental changes to the probe format.
def seeded(path):
 rows=[]
 for line in path.read_text().splitlines():
  if line.startswith('cases'):continue
  a=line.split();assert len(a)==19,a
  rows.append(dict(shape=float(a[0]),n=int(a[1]),rep=int(a[2]),status=int(a[4]),work=int(a[6]),budget=int(a[8]),coef=list(map(float,a[10:13])),ll=float(a[14]),restricted=int(a[16]),seconds=float(a[18])))
 return rows
bs=seeded(p/'baseline-seeded.txt');ass=seeded(p/'final-seeded.txt');assert len(bs)==len(ass)==189
max_error=0
for b,a in zip(bs,ass):
 assert (a['shape'],a['n'],a['rep'])==(b['shape'],b['n'],b['rep'])
 assert a['status']==b['status']==0 and a['budget']==b['budget']==0 and a['restricted']==b['restricted'],(a,b)
 for i,(x,y) in enumerate(zip(a['coef'],b['coef'])):
  scale=max(abs(y),b['coef'][1]) if i==0 else b['coef'][1] if i==1 else 1
  err=abs(x-y)/scale;max_error=max(max_error,err);assert err<=1e-10,(a,b,i,err)
 assert abs(a['ll']-b['ll'])<=1e-10*max(1,abs(b['ll'])),(a,b)
assert (p/'baseline-controls.txt').read_bytes()==(p/'final-controls.txt').read_bytes()
sizes=[]
for n in sorted({r['n'] for r in bs}):
 b=[r for r in bs if r['n']==n];a=[r for r in ass if r['n']==n]
 sizes.append(dict(n=n,cases=len(a),before_seconds=sum(r['seconds'] for r in b),after_seconds=sum(r['seconds'] for r in a),before_work=sum(r['work'] for r in b),after_work=sum(r['work'] for r in a),max_after_seconds=max(r['seconds'] for r in a)))
summary=dict(benchmarks=rows,seeded=dict(cases=len(ass),before_seconds=sum(r['seconds'] for r in bs),after_seconds=sum(r['seconds'] for r in ass),before_work=sum(r['work'] for r in bs),after_work=sum(r['work'] for r in ass),before_max_work=max(r['work'] for r in bs),after_max_work=max(r['work'] for r in ass),maximum_coefficient_error_in_contract_units=max_error,sizes=sizes),control_cases=28)
(p/'summary.json').write_text(json.dumps(summary,indent=2)+'\n')
print(json.dumps(summary,indent=2))
