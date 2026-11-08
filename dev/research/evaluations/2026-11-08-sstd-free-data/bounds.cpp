#include "cppunivariate/distributions/sstd/mle.hpp"
#include <iostream>
#include <iomanip>
int main(){std::cout<<std::setprecision(17);const std::vector<double>x{-2,-1,-.4,0,.2,.3,.5,1,2,5};for(double nu:{2.1,3.,5.,20.}){double shift=0;cppunivariate::dens::sstd_sigma(nu,2,shift);for(int k=0;k<25;++k){const double l=k/25.,h=(k+1)/25.;const auto b=cppunivariate::sstd_skew_cert::bounds({x.data(),x.size()},1,nu,shift/1.5,l,h);std::cout<<nu<<" "<<l<<" "<<h<<" "<<b.value.lo<<" "<<b.value.hi<<" "<<b.score.lo<<" "<<b.score.hi<<"\n";}}}
