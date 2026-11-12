from pathlib import Path
import mpmath as m,json,subprocess,signal
def deadline(signum,frame):raise TimeoutError("root exceeded 60 seconds")
signal.signal(signal.SIGALRM,deadline)
P=Path(__file__).resolve().parent;root=P.parents[3];m.mp.dps=100
x=[-m.log(1-(m.mpf(i)+m.mpf('.5'))/20) for i in range(20)]
def constraints(a,b):
 z=[m.exp(a),*x[1:-1],m.exp(b)];mean=m.fsum(z)/20
 return m.fsum((v/mean)**2 for v in z)/40-1,1+m.fsum(m.log(v) for v in z)/20-m.fsum(v*m.log(v) for v in z)/m.fsum(z)
a,b=m.findroot(constraints,(m.log(x[0]),m.log(4.5)),tol=m.mpf('1e-90'),maxsteps=100);x[0]=m.exp(a);x[-1]=m.exp(b)
results=[]
source=root/'build/scratch/coupled-burr.cpp';source.write_text('''#include <cppunivariate/distributions/burr/mle.hpp>
#include <iostream>
#include <iomanip>
int main(){int n;double k,b,s;std::cin>>n>>k>>b>>s;std::vector<double>x(n);for(auto&v:x)std::cin>>v;bool ok=cppunivariate::burr_local_accuracy({x.data(),x.size()},k,b,s,{{true,true,true}},1e-10,1000);std::cout<<std::setprecision(17)<<ok<<" "<<k<<" "<<b<<" "<<s<<"\\n";}
''')
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
for mode in ['native','double']:
 cmd=['g++','-O2','-std=c++17','-I'+str(root/'univariateML-cpp/include'),'-I'+boost,str(source),'-o',str(root/('build/scratch/coupled-burr-'+mode))]
 if mode=='double':cmd.insert(1,'-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS')
 subprocess.run(cmd,check=True)
for delta in ['1e-6','1e-8']:
 for direction in [1,-1]:
  raw=[float(v) for v in x];raw[-1]=float(x[-1]*(1+direction*m.mpf(delta)));z=list(map(m.mpf,raw));mean=m.fsum(z)/20
  def likelihood(u,v,D):
   k=m.exp(u);c=m.exp(v)
   return m.fsum(v+D+c*m.log(t)-(k+1)*m.log1p(m.exp(D+c*m.log(t)-u)) for t in z)
  def g(u,v,D):return m.exp(2*u)*m.diff(lambda w:likelihood(w,v,D),u),m.diff(lambda w:likelihood(u,w,D),v),m.diff(lambda w:likelihood(u,v,w),D)
  try:
   signal.alarm(60)
   u,v,D=m.findroot(g,(m.log(1/m.mpf(delta)),0,-m.log(mean)),tol=m.mpf('1e-75'),maxsteps=100)
   k=m.exp(u);c=m.exp(v);s=m.exp((u-D)/c)
   point=[u,v,D];H=m.matrix(3)
   for i in range(3):
    for j in range(3):
     def shifted(da,db):return likelihood(*(point[t]+(da if t==i else 0)+(db if t==j else 0) for t in range(3)))
     H[i,j]=-m.diff(shifted,(m.mpf(0),m.mpf(0)),(1,1))
   signal.alarm(0)
   if not (H[0,0]>0 and m.det(H[:2,:2])>0 and m.det(H)>0):continue
  except Exception as e:signal.alarm(0);print('root skipped',delta,direction,str(e)[:160],flush=True);continue
  entry=dict(delta=delta,direction=direction,data=raw,reference=dict(shape1=m.nstr(k,85),shape2=m.nstr(c,85),scale=m.nstr(s,85)),builds={})
  for mode in ['native','double']:
   line=' '.join(map(str,[20,float(k*(1+m.mpf('1e-5'))),float(c*(1+m.mpf('1e-8'))),float(s*(1+m.mpf('1e-5'))),*raw]))+'\n'
   out=subprocess.check_output([str(root/('build/scratch/coupled-burr-'+mode))],input=line,text=True).split();entry['builds'][mode]=dict(status=out[0],coefficients=out[1:],max_error=max(float(abs(m.mpf(float(got))-truth)/truth) for got,truth in zip(out[1:],[k,c,s])))
  results.append(entry);print(delta,direction,entry['builds'],flush=True);(P/'coupled-burr-probe.json').write_text(json.dumps(results,indent=2)+'\n')
print('finite local maxima',len(results),flush=True)

assert len(results)==2
assert all(b["status"]=="1" and b["max_error"]<=1e-10 for r in results for b in r["builds"].values())
