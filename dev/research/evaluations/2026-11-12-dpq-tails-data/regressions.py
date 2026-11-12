"""Recreate independent 100-digit per-observation density regressions.
Run through /tmp/umvl-cpu/run after the final seam replay; --write appends missing tests.
"""
import json,sys
from pathlib import Path
import mpmath as mp
root=Path(__file__).resolve().parents[4]
mp.mp.dps=100
rows=[r for r in map(json.loads,(Path(__file__).parent/'final.jsonl').read_text().splitlines()) if r['index'] in [75,4435,4438,4601,4604,4611,5665,5666,5668,5669]]
assert len(rows)==10
rows.append(dict(family='dgamma',index=997,parameters=[56054383409.529709,56054.097533632288],data=[1000000,1000001,1000003,1000008,1000011]))
results=[]
for row in rows:
 f=row['family'];p=list(map(mp.mpf,row['parameters']));expected=[]
 for v in row['data']:
  x=mp.mpf(v)
  if f=='lnorm':
   a,b=p; y=mp.log(x);ld=-(y-a)**2/(2*b*b)-mp.log(b)-mp.log(2*mp.pi)/2-y
  elif f=='lgamma':
   a,b=p;y=mp.log(x);ld=a*mp.log(b)+(a-1)*mp.log(y)-b*y-mp.loggamma(a)-y
  elif f=='bbinom':
   n,a,b=p;ld=mp.loggamma(n+1)-mp.loggamma(x+1)-mp.loggamma(n-x+1)+mp.log(mp.beta(a+x,b+n-x)/mp.beta(a,b))
  elif f=='pareto4':
   a,b,s,m=p;z=mp.log((x-m)/s);ld=mp.log(a*b/s)+(b-1)*z-(1+a)*mp.log1p(mp.exp(b*z))
  elif f=='dgamma':
   a,b=p;mid=x+mp.mpf('.5');base=(a-1)*mp.log(mid)+a*mp.log(b)-b*mid-mp.loggamma(a)
   ld=base+mp.log(mp.quad(lambda u:mp.exp((a-1)*mp.log1p(u/mid)-b*u),[-mp.mpf('.5'),mp.mpf('.5')]))
  expected.append(float(ld))
 test='\nTEST_CASE("'+f+': archived density seam '+str(row['index'])+'") {\n'
 test+='  const std::vector<double> data{'+','.join(repr(float(v)) for v in row['data'])+'};\n'
 test+='  const std::vector<double> expected{'+','.join(map(repr,expected))+'};\n'
 test+='  for(std::size_t i=0;i<data.size();++i) {\n'
 test+='    const double actual=cppunivariate::distributions::d'+f+'(data[i],'+','.join(map(repr,row['parameters']))+',true);\n'
 test+='    CHECK(std::abs(actual-expected[i])<=1e-10*std::max(1.0,std::abs(expected[i])));\n  }\n}\n'
 path=root/f'univariateML-cpp/include/cppunivariate/distributions/{f}/test.cpp'
 if test not in path.read_text():
  assert '--write' in sys.argv, ('missing regression',f,row['index'])
  with path.open('a') as out:out.write(test)
 results.append(dict(family=f,index=row['index'],parameters=row['parameters'],data=row['data'],expected=expected))
(Path(__file__).parent/'density-references.json').write_text(json.dumps(results,indent=2)+'\n')
