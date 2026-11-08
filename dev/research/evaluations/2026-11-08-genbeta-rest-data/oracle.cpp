#include "cppunivariate/distributions/genbeta/mle.hpp"
#include <iostream>
int main(){int n;std::cin>>n;std::vector<double>x(n);for(double&v:x)std::cin>>v;std::cout.precision(17);for(double q:{.01,.1,.3}){const auto r=cppunivariate::genbeta_mle({x.data(),x.size()},1,1e-7,300,NAN,q);std::cout<<q<<" "<<r.shape1<<" "<<r.shape3<<" "<<r.logLik<<" "<<int(r.status)<<" "<<int(r.certificate_failure)<<" "<<r.iterations<<"\n";}}
