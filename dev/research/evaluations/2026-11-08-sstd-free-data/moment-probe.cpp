#include "cppunivariate/distributions/sstd/mle.hpp"
#include <iostream>
#include <iomanip>
int main(){std::cout<<std::setprecision(17);for(double nu:{20.,127.,128.,1e4,1e8,1e12,1e16,1e17}){double shift=0;cppunivariate::dens::sstd_sigma(nu,2,shift);std::cout<<nu<<" "<<shift/1.5<<"\n";}}
