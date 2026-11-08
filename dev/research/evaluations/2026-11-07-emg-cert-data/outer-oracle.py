"""70-digit independent physical score roots; no production Mills evaluator."""
import csv,json,sys
from pathlib import Path
import mpmath as m
m.mp.dps=70
p=Path(__file__).resolve().parent
suffix='-double' if '--double' in sys.argv else ''
g=json.loads(Path('univariateML-cpp/include/cppunivariate/distributions/emg/golden.json').read_text())
x=[m.mpf(float(v)) for v in g['data']]
answers=[]
for row in csv.DictReader((p/f'outer{suffix}.csv').open()):
    mask=int(row['mask']);free=[i for i in range(3) if not mask&(1<<i)]
    base=[m.mpf(float(row['mu'])),m.log(m.mpf(float(row['sigma']))),m.log(m.mpf(float(row['lambda'])))];n=len(x)
    def score(*v):
        theta=base.copy()
        for j,t in zip(free,v):theta[j]=t
        mu,s,l=theta[0],m.exp(theta[1]),m.exp(theta[2]);k=s*l
        acc=[m.mpf(0),m.mpf(0),m.mpf(0)]
        for y in x:
            z=(y-mu)/s;t=k-z
            r=m.exp(-t*t/2)*m.sqrt(2/m.pi)/m.erfc(t/m.sqrt(2));q=k-r
            acc[0]+=q/s;acc[1]+=q*(k+z)-k*z;acc[2]+=1+k*(q-z)
        return tuple(acc[j] for j in free)
    root=m.findroot(score,tuple(base[j] for j in free),tol=m.mpf('1e-58'),maxsteps=20)
    theta=base.copy()
    for j,t in zip(free,root):theta[j]=t
    truth=[theta[0],m.exp(theta[1]),m.exp(theta[2])]
    errors=[]
    assert row['status']=='0' and row['root']=='1' and row['accurate']=='1',row
    for j,name in enumerate(['mu','sigma','lambda']):
        value=m.mpf(float(row[name]));lo=m.mpf(float(row[name+'_lo']));hi=m.mpf(float(row[name+'_hi']))
        assert lo<=truth[j]<=hi,(mask,name,str(truth[j]),str(lo),str(hi))
        error=abs(value-truth[j])/max(abs(truth[j]),truth[1] if j==0 else 0)
        if j in free:assert error<=m.mpf('1e-10'),(mask,name,str(error))
        errors.append(str(error))
    answers.append(dict(held_mask=mask,root=[str(v) for v in truth],relative_errors=errors))
(p/f'outer-oracle{suffix}.json').write_text(json.dumps(answers,indent=2)+'\n')
print('7 independent physical coefficient root/enclosure checks passed',suffix)
