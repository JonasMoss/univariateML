import json,mpmath as m,os
from pathlib import Path
m.mp.dps=90
x=list(map(m.mpf,[-3,-1,0,1,2]))
def cdf(z):return m.erfc(-z/m.sqrt(2))/2
def f(mu):return m.fsum(m.log(cdf(v-mu+2)-cdf(v-mu-2)) for v in x)
mu=m.findroot(lambda t:m.diff(f,t),(-m.mpf('.4'),0),tol=m.mpf('1e-85'))
nearest=float(mu);error=abs(m.mpf(nearest)-mu);limit=m.mpf('1e-30')*max(abs(mu),1)
r={'reference':m.nstr(mu,85),'nearest_binary64':repr(nearest),'distance':m.nstr(error,75),'target':m.nstr(limit,20),'unreachable':bool(error>limit),'information':m.nstr(-m.diff(f,mu,2),30)}
radius=m.mpf('1e-60');left,right=mu-radius,mu+radius;sl,sr=m.diff(f,left),m.diff(f,right)
assert sl>0 and sr<0
lower=min(abs(m.mpf(nearest)-left),abs(m.mpf(nearest)-right));assert lower>limit
r.update(enclosure=[m.nstr(left,85),m.nstr(right,85)],enclosure_scores=[m.nstr(sl,75),m.nstr(sr,75)],quantization_lower_bound=m.nstr(lower,75),uniqueness='Strict concavity of the held-scale, held-width location likelihood.')
path=Path(__file__).with_name('bhatt-quantization.json');temporary=path.with_name(path.name+'.tmp-'+str(os.getpid()));temporary.write_text(json.dumps(r,indent=2)+'\n');temporary.replace(path);print(json.dumps(r,indent=2))
