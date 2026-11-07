#!/usr/bin/env python3
import json,pathlib,subprocess,sys
p=pathlib.Path('univariateML-cpp/include/cppunivariate/distributions/tlambda/golden.json')
g=json.loads(p.read_text());names=['mu','sigma','lambda'];truth=[g['estimates'][k] for k in names]
text=' '.join(map(str,[len(g['data'])]+truth+g['data']))+'\n'
raw=subprocess.check_output([sys.argv[1]],input=text,text=True)
rows=[]
for line in raw.splitlines():
    v=line.split();mask,status,certified=map(int,v[:3]);coef=list(map(float,v[3:6]));ll=float(v[6])
    assert status==0,line
    for a,b in zip(coef,truth):assert abs(a-b)<=1e-6*(1+abs(b)),line
    assert abs(ll-g['logLik'])<=1e-7,line
    assert mask==0 or certified==1,line
    rows.append(dict(mask=mask,certified=bool(certified),coefficients=dict(zip(names,coef)),
                     coefficient_abs_drift=max(abs(a-b) for a,b in zip(coef,truth)),
                     loglik_abs_drift=abs(ll-g['logLik']),iterations=int(v[7]),seconds=float(v[8])))
result=dict(n=len(g['data']),tolerance='1e-6*(1+abs(coef)), 1e-7 absolute loglik',passed=True,rows=rows)
pathlib.Path(__file__).with_suffix('.json').write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(result))
