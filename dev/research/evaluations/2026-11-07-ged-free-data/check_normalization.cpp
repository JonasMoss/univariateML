#include "cppunivariate/distributions/ged/mle.hpp"
#include <iostream>
#include <iomanip>
int main(){std::vector<double>x;double v;while(std::cin>>v)x.push_back(1e16+10*v);auto r=cppunivariate::ged_mle({x.data(),x.size()},1.220703e-4,200);auto actual=cppunivariate::ged_detail::conditional({x.data(),x.size()},r.nu,r.mu,r.sigma,200);std::cout<<std::setprecision(17)<<"status "<<int(r.status)<<" certified "<<r.certified<<" mean "<<r.mu<<" sd "<<r.sigma<<" nu "<<r.nu<<" logLik "<<r.logLik<<" actual "<<actual.logLik<<" discrepancy "<<r.logLik-actual.logLik<<"\n";}
