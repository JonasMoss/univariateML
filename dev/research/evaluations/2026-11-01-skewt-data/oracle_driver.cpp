#include "cppunivariate/distributions/st/mle.hpp"
#include <iostream>
#include <iomanip>
#include <vector>
int main(){int n;while(std::cin>>n){std::vector<double>x(n);for(double&v:x)std::cin>>v;auto f=cppunivariate::st_mle(cppunivariate::span{x.data(),x.size()},1e-6,200);std::cout<<int(f.status)<<' '<<std::setprecision(17)<<f.logLik<<' '<<f.xi<<' '<<f.omega<<' '<<f.alpha<<' '<<f.nu<<' '<<f.reason<<std::endl;}}
