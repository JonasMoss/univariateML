#include "cppunivariate/distributions/sged/mle.hpp"
#include <chrono>
#include <iostream>
#include <iomanip>
int main(){std::vector<double>x{-3,-1,-.2,0,.3,.4,2}; for(double nu:{.3,.5,.9})for(double xi:{.3,1.3,3.}){auto start=std::chrono::steady_clock::now();cppunivariate::SgedResult r{};for(int k=0;k<20;++k)r=cppunivariate::sged_mle(cppunivariate::span{x.data(),x.size()},1e-8,200,.1,NAN,nu,xi);std::cout<<std::setprecision(17)<<nu<<" "<<xi<<" "<<int(r.status)<<" "<<r.sigma<<" "<<r.logLik<<" "<<r.certified<<" "<<std::chrono::duration<double,std::micro>(std::chrono::steady_clock::now()-start).count()/20<<"\n";}}
