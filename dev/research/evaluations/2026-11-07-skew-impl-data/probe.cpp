#include "cppunivariate/distributions/sstd/mle.hpp"
#include "cppunivariate/distributions/st/mle.hpp"
#include <iostream>
#include <iomanip>
int main(){int family,n;double first,sd,nu,skew;
 std::cout<<std::setprecision(17);
 while(std::cin>>family>>n>>first>>sd>>nu>>skew){std::vector<double>x(n);for(double& v:x)std::cin>>v;cppunivariate::span s{x.data(),x.size()};
 if(family<2){auto r=cppunivariate::sstd_mle(s,1e-6,200,family==0?NAN:first,family==0?sd:NAN,nu,skew);std::cout<<int(r.status)<<" "<<r.certified<<" "<<(family==0?r.mu:r.sigma)<<" "<<r.logLik<<"\n";}
 else {auto r=cppunivariate::st_mle(s,1e-6,200,first,sd,NAN,nu);std::cout<<int(r.status)<<" "<<r.certified<<" "<<r.alpha<<" "<<r.logLik<<"\n";}
 }
}
