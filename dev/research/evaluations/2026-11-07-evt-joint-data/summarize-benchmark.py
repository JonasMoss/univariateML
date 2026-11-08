#!/usr/bin/env python3
import json, math, pathlib, statistics, mpmath as mp
root=pathlib.Path(__file__).resolve().parent
repo=root.parents[3]
rows={}
for phase in ['before','after']:
    batches=[]
    for j in range(1,4):
        batches.append([line.split() for line in (root/f'{phase}-{j}.txt').read_text().splitlines()])
    rows[phase]=batches
for j in [0,3,5,6]:
    assert rows['before'][0][j][:6]==rows['after'][0][j][:6],('default changed',j)
for family,j,names in [('gev',5,['mu','sigma','xi']),('frechet',6,['lambda','mu','sigma'])]:
    golden=json.loads((repo/f'univariateML-cpp/include/cppunivariate/distributions/{family}/golden.json').read_text())
    values=list(map(float,rows['after'][0][j][2:6]))
    assert int(rows['after'][0][j][1])==0
    for actual,expected in zip(values,[golden['estimates'][name] for name in names]+[golden['logLik']]):
        assert math.isclose(actual,expected,rel_tol=1e-10,abs_tol=1e-12),(family,actual,expected)
# Independent 50-digit scalar score root under the exact held public coefficients.
golden=json.loads((repo/'univariateML-cpp/include/cppunivariate/distributions/gev/golden.json').read_text())
mp.mp.dps=50
mu=mp.mpf(golden['estimates']['mu']);sigma=mp.mpf(golden['estimates']['sigma'])
z=[(mp.mpf(x)-mu)/sigma for x in golden['data']]
def score(k):
    ans=mp.mpf(0)
    for v in z:
        q=k*v;L=mp.log1p(q)/k
        d=(q/(1+q)-mp.log1p(q))/(k*k)
        ans+=-L+(mp.exp(-L)-1-k)*d
    return ans
actual=float(rows['after'][0][7][4]);assert int(rows['after'][0][7][1])==0
reference=mp.findroot(score,(actual*.99,actual*1.01))
relative_error=abs((mp.mpf(actual)-reference)/reference)
assert relative_error<=mp.mpf('1e-10'),relative_error
output=dict(defaults_bit_identical=True,default_golden_tolerance=1e-10,
    scalar_reference=str(reference),scalar_relative_error=float(relative_error),timings=[])
for j,label in enumerate(['GEV default n=7','GEV shape-only n=7','GEV half tie n=4',
    'Frechet default n=7','Frechet half tie n=4','GEV default n=500',
    'Frechet default n=500','GEV shape-only n=500','GEV two-point n=10001']):
    row=dict(workload=label)
    for phase in ['before','after']:
        row[phase+'_us']=statistics.median(float(batch[j][6]) for batch in rows[phase])
        row[phase+'_status']=int(rows[phase][0][j][1])
    output['timings'].append(row)
reference_sigma=1/10001
for phase in ['before','after']:
    value=float(rows[phase][0][8][3])
    output[phase+'_two_point_sigma_relative_error']=abs(value/reference_sigma-1)
assert int(rows['after'][0][8][1])==0
assert output['after_two_point_sigma_relative_error']<=1e-10
print(json.dumps(output,indent=2))
