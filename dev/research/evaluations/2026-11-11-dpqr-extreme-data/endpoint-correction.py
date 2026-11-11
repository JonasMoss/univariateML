from pathlib import Path
import mpmath as mp,json
mp.mp.dps=160
p=Path('univariateML-cpp/include/cppunivariate/distributions/fpareto/truth.json');t=json.loads(p.read_text());records=[]
for row in t['values']:
 if float(row[1])==0 and float(t['parameters'][row[0]][-1])<0:
  b,c,a,s,mu=map(mp.mpf,t['parameters'][row[0]]);v=-mu;z=c*mp.log(v/s)
  value=mp.log(c)-mp.log(v)-mp.log(mp.beta(a,b))-a*mp.log1p(mp.exp(-z))-b*mp.log1p(mp.exp(z));row[2]=float(value);records.append(dict(parameters=t['parameters'][row[0]],logdensity=str(value)))
p.write_text(json.dumps(t,indent=2,allow_nan=False)+'\n')
a=mp.mpf(1e-25);q=mp.mpf(1e-200);z=a*q*q;expected=mp.log(mp.gammainc(a,z,mp.inf)/mp.gamma(a));records.append(dict(family='naka',parameters=[1e-25,1.],x=1e-200,logupper=str(expected)))
Path('dev/research/evaluations/2026-11-11-dpqr-extreme-data/endpoint-and-tinyshape-reference.json').write_text(json.dumps(records,indent=2)+'\n')
print(records)
