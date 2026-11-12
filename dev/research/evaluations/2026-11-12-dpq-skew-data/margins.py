"""Independent 240-digit check of the propagated moment/coordinate margins."""
import study,check,subprocess,mpmath as mp,json,math
mp.mp.dps=240
src='''#include "cppunivariate/special/skew_coordinate.hpp"
#include <iostream>
#include <iomanip>
#include <string>
int main(){std::cout<<std::setprecision(17);std::string f;double x,mean,sd,nu,xi;
while(std::cin>>f>>x>>mean>>sd>>nu>>xi){const auto m=f=="sged"?cppunivariate::special::skew_coordinate::ged(nu):cppunivariate::special::skew_coordinate::student(nu);const auto c=cppunivariate::special::skew_coordinate::coordinate(x,mean,sd,xi,m);
std::cout<<m.value.high<<" "<<m.value.low<<" "<<m.value.tiny<<" "<<m.error<<" "<<c.radius.high<<" "<<c.radius.low<<" "<<c.radius.tiny<<" "<<c.arithmetic_error<<" "<<c.valid<<"\\n";}}
'''
(study.W/'margins.cpp').write_text(src)
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
subprocess.run(['c++','-O2','-std=c++17','-I'+str(study.ROOT/'univariateML-cpp/include'),'-I'+boost,str(study.W/'margins.cpp'),'-o',str(study.W/'margins')],check=True)
jobs=[];seen=set()
for r in check.jobs():
 j=r['job'];key=json.dumps([j[0],j[2],j[5]])
 if r['source']=='skew' and key not in seen:jobs.append(j);seen.add(key)
lines=[' '.join(map(str,[j[0],j[2],*j[5]])) for j in jobs]
output=subprocess.check_output([str(study.W/'margins')],input='\n'.join(lines)+'\n',text=True).splitlines();records=[]
for j,line in zip(jobs,output):
 v=[mp.mpf(float(z)) for z in line.split()];x=mp.mpf(j[2]);mean,sd,nu,xi=map(mp.mpf,j[5])
 if j[0]=='sged':m=mp.exp(mp.loggamma(2/nu)-(mp.loggamma(1/nu)+mp.loggamma(3/nu))/2)
 else:m=mp.sqrt((nu-2)/mp.pi)*mp.exp(mp.loggamma((nu-1)/2)-mp.loggamma(nu/2))
 t=min(xi,1/xi);s=mp.sqrt((1-m*m)*(1+t**4)+(2*m*m-1)*t*t);w=(x-mean)/sd*(1 if xi<=1 else -1);n=w*s-m*(1-t*t);radius=abs(n/(t*t if n>=0 else 1))
 em=abs(sum(v[:3])-m);ec=abs(sum(v[4:7])-radius)
 records.append(dict(family=j[0],nu=float(nu),xi=float(xi),moment_ratio=float(em/v[3]),coordinate_ratio=float(ec/v[7])))
result=dict(checks=len(records),worst_moment_ratio=max(r['moment_ratio'] for r in records),worst_coordinate_ratio=max(r['coordinate_ratio'] for r in records),failures=[r for r in records if max(r['moment_ratio'],r['coordinate_ratio'])>1])
(study.H/'margins.json').write_text(json.dumps(result,indent=2)+'\n');print(result)
assert not result['failures']
