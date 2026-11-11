"""Fresh independent all-free SSTD root for the unchanged default golden.

Run through /tmp/umvl-cpu/run, after the required suites. The direct density
and mpmath numerical derivatives are independent of production derivatives.
This supplements the 686-row replay, without altering frozen golden files.
"""
import json
from pathlib import Path
import mpmath as m
from oracle import objective

D=Path(__file__).resolve().parent
root=D.parents[3]
m.mp.dps=90
golden=json.loads((root/'univariateML-cpp/include/cppunivariate/distributions/sstd/golden.json').read_text())
keys=['mean','sd','nu','xi']
def row(mode):
    return next(r for r in map(json.loads,(D/f'golden-{mode}.jsonl').read_text().splitlines()) if r['index']==40001)

sample=golden['data']
tokens=next(s.split() for s in (D/'golden-inputs.txt').read_text().splitlines() if s.split()[1]=='sstd')
assert int(tokens[2])==len(sample)==500
assert list(map(float,tokens[7:]))==sample
baseline=row('before-native')
assert not baseline['error']
start=[m.mpf(baseline['estimates'][k]) for k in keys]
origin,unit=start[:2]
y=[(m.mpf(v)-origin)/unit for v in sample]
q=[m.mpf(0),m.mpf(0),m.log(start[2]),m.log(start[3])]
def decode(q):return [q[0],m.exp(q[1]),m.exp(q[2]),m.exp(q[3])]
magnitude=max(1,abs(objective('sstd',y,decode(q))))
def cost(*q):return -objective('sstd',y,decode(q))/magnitude
def score(*q):
    return tuple(m.diff(cost,tuple(q),tuple(int(k==j) for k in range(4))) for j in range(4))
q=list(m.findroot(score,tuple(q),solver='mdnewton',tol=m.mpf('1e-75'),maxsteps=40))
h=m.matrix([[m.diff(cost,tuple(q),tuple(int(k==i)+int(k==j) for k in range(4))) for j in range(4)] for i in range(4)])
m.cholesky(h)
correction=m.lu_solve(h,m.matrix(score(*q)))
reference_error=max(map(abs,correction))
assert reference_error<m.mpf('1e-55'),reference_error
target=decode(q);target[0]=origin+unit*target[0];target[1]*=unit
checks={}
for mode in ['before-native','before-double','after-native','after-double']:
    r=row(mode);assert not r['error']
    errors={k:float(abs(m.mpf(r['estimates'][k])-v)/max(abs(v),target[1] if j==0 else 0)) for j,(k,v) in enumerate(zip(keys,target))}
    assert max(errors.values())<=1e-10,(mode,errors)
    checks[mode]=dict(components=errors,maximum_scaled_error=max(errors.values()))
for mode in ['after-r','after-wasm']:
    path=D/f'golden-sstd-{mode}.json'
    if not path.exists():continue
    r=json.loads(path.read_text());assert not r['error'],r
    errors={k:float(abs(m.mpf(float(r['estimates'][k]))-v)/max(abs(v),target[1] if j==0 else 0)) for j,(k,v) in enumerate(zip(keys,target))}
    assert max(errors.values())<=1e-10,(mode,errors)
    checks[mode]=dict(components=errors,maximum_scaled_error=max(errors.values()))
out=dict(family='sstd',index=40001,seed=golden['seed'],n=len(sample),held={},precision_digits=90,
         method='direct density, mpmath numerical derivatives; independent log-nu coordinate',
         theta={k:str(v) for k,v in zip(keys,target)},positive_information=True,
         reference_coordinate_error=str(reference_error),
         logLik=str(-objective('sstd',y,decode(q))-len(y)*m.log(unit)),checks=checks)
(D/'golden-sstd-verification.json').write_text(json.dumps(out,indent=2)+'\n')
print('all-free sstd golden seed',golden['seed'],'90-digit reference PASS')
for mode,r in checks.items():print(mode,r['maximum_scaled_error'])
