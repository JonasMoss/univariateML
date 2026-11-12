"""Independent 100-digit stationary points with two large beta-prime shapes."""
from pathlib import Path
import json,subprocess,mpmath as m,signal
P=Path(__file__).resolve().parent;root=P.parents[3];m.mp.dps=100
signal.signal(signal.SIGALRM,lambda *_:(_ for _ in ()).throw(TimeoutError('reference exceeded 60 s')))
z=[m.sqrt(2)*m.erfinv(2*(m.mpf(i)+m.mpf('.5'))/20-1) for i in range(20)];sd=m.sqrt(m.fsum(v*v for v in z)/20);z=[v/sd for v in z]
source=root/'build/scratch/comparable-beta.cpp';source.write_text('''#include <cppunivariate/distributions/betapr/mle.hpp>
#include <iostream>
#include <iomanip>
int main(){int n;double p,q,s;std::cin>>n>>p>>q>>s;std::vector<double>x(n);for(auto&v:x)std::cin>>v;cppunivariate::BetaPrimeResult r{p,q,s,0,0,cppunivariate::BetaPrimeStatus::Ok};r.certified=false;r=cppunivariate::betapr_detail::accurate_fallback(r,{x.data(),x.size()},NAN,NAN,NAN,1e-10,1000);std::cout<<std::setprecision(17)<<(r.status==cppunivariate::BetaPrimeStatus::Ok)<<" "<<r.alpha<<" "<<r.beta<<" "<<r.scale<<"\\n";}
''')
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
for mode in ['native','double']:
 cmd=['g++','-O2','-std=c++17','-I'+str(root/'univariateML-cpp/include'),'-I'+boost,str(source),'-o',str(root/('build/scratch/comparable-beta-'+mode))]
 if mode=='double':cmd.insert(1,'-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS')
 subprocess.run(cmd,check=True)
rows=[]
for spread in ['.01','.003']:
 raw=[float(m.exp(m.mpf(spread)*v)) for v in z];x=list(map(m.mpf,raw));guess=2/m.mpf(spread)**2
 def likelihood(a,b,c):
  p=m.exp(a);q=m.exp(b);s=m.exp(c)
  return -20*(m.loggamma(p)+m.loggamma(q)-m.loggamma(p+q))+m.fsum(p*(m.log(v)-c)-(p+q)*m.log1p(v/s) for v in x)
 def score(a,b,c):return m.diff(lambda t:likelihood(t,b,c),a),m.diff(lambda t:likelihood(a,t,c),b),m.diff(lambda t:likelihood(a,b,t),c)
 signal.alarm(60);a,b,c=m.findroot(score,(m.log(guess),m.log(guess),0),tol=m.mpf('1e-75'),maxsteps=100);signal.alarm(0)
 pp,qq,ss=m.exp(a),m.exp(b),m.exp(c);point=[a,b,c];H=m.matrix(3)
 for i in range(3):
  for j in range(3):
   def shifted(da,db):return likelihood(*(point[t]+(da if t==i else 0)+(db if t==j else 0) for t in range(3)))
   H[i,j]=-m.diff(shifted,(m.mpf(0),m.mpf(0)),(1,1))
 assert H[0,0]>0 and m.det(H[:2,:2])>0 and m.det(H)>0
 r=dict(spread=spread,data=raw,reference=dict(shape1=m.nstr(pp,85),shape2=m.nstr(qq,85),scale=m.nstr(ss,85)),digits=100,builds={})
 for mode in ['native','double']:
  line=' '.join(map(str,[20,float(pp*(1+m.mpf('1e-5'))),float(qq*(1-m.mpf('1e-5'))),float(ss*(1+m.mpf('1e-5'))),*raw]))+'\n'
  out=subprocess.check_output([str(root/('build/scratch/comparable-beta-'+mode))],input=line,text=True).split();r['builds'][mode]=dict(status=out[0],coefficients=out[1:],max_error=max(float(abs(m.mpf(float(got))-truth)/truth) for got,truth in zip(out[1:],[pp,qq,ss])))
 rows.append(r);(P/'comparable-beta-probe.json').write_text(json.dumps(rows,indent=2)+'\n');print(spread,r['builds'],flush=True)

assert len(rows)==2
assert all(b["status"]=="1" and b["max_error"]<=1e-10 for r in rows for b in r["builds"].values())
