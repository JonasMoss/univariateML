"""Reproduce a named finite precision region requiring more centering bits."""
import json,sys,math
from pathlib import Path
import mpmath as mp
HERE=Path(__file__).resolve().parent;ROOT=HERE.parents[3]
sys.path.insert(0,str(HERE));import audit
mp.mp.dps=200
xi=mp.mpf(1e-12);m=mp.sqrt(2/mp.pi);mu=m*(xi-1/xi)
sigma=mp.sqrt((1-m*m)*(xi*xi+1/(xi*xi))+2*m*m-1)
root=-mu/sigma;x=float(root);location=float(mp.mpf(x)-root)
params=[location,1.,float(xi)]
z=(mp.mpf(x)-mp.mpf(location))*sigma+mu;v=z/(xi if z>=0 else 1/xi)
record=dict(family='snorm',x=x,parameters=params,exact_standardized_residual=str(v))
a=audit.evaluate([('snorm',1,x,False,True,params)])[0]
record['actual']='NaN' if math.isnan(a) else a
(HERE/'skew-precision-region.json').write_text(json.dumps(record,indent=2)+'\n')
print(record,flush=True);assert math.isnan(a)
p=ROOT/'univariateML-cpp/include/cppunivariate/distributions/snorm/test.cpp';title='snorm: named compensated-centering precision region'
if title not in p.read_text():p.write_text(p.read_text()+'\nTEST_CASE("'+title+'") {\n CHECK(std::isnan(cppunivariate::distributions::psnorm('+','.join(map(repr,[x,*params]))+',false,true)));\n}\n')
