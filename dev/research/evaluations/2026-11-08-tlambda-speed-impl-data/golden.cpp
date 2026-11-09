#include "cppunivariate/distributions/tlambda/mle.hpp"
#include <iostream>
#include <iomanip>
int main(){int n;std::cin>>n;std::vector<double>x(n);for(double&v:x)std::cin>>v;std::cout<<std::setprecision(17);
for(int mask:{0,1,2}){auto f=cppunivariate::tlambda_mle({x.data(),x.size()},1e-10,2000,NAN,mask==1?0.04752470769410265657:NAN,mask==2?1.0988151608485910244:NAN);std::cout<<mask<<' '<<int(f.status)<<' '<<f.mu<<' '<<f.sigma<<' '<<f.lambda<<' '<<f.logLik<<'\n';}}
