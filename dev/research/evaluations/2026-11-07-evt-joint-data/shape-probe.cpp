#include "cppunivariate/distributions/gev/mle.hpp"
#include <iostream>
int main(){for(std::vector<double> x: {std::vector<double>{-.5,-.3,0,.2,.5},std::vector<double>{.1,.2,.3},std::vector<double>{-.3,-.2,-.1},std::vector<double>{0,0,0}}){auto f=cppunivariate::gev_mle({x.data(),x.size()},1e-10,100,NAN,0,1);std::cout<<int(f.status)<<" "<<f.xi<<" "<<f.logLik<<" "<<f.iterations<<"\n";}}
