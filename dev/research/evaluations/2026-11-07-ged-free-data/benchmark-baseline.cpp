#include "baseline-mle.hpp"
#include <chrono>
#include <iomanip>
#include <iostream>
#include <vector>
int main(){std::vector<double>x;double v;while(std::cin>>v)x.push_back(v);const double nan=NAN;for(int mask=0;mask<4;++mask){auto t=std::chrono::steady_clock::now();cppunivariate::GedResult r{};for(int i=0;i<20;++i)r=cppunivariate::ged_mle({x.data(),x.size()},1.220703e-4,200,mask&1?-.017698887677594307:nan,mask&2?1.0178559902092952:nan,nan);std::cout<<std::setprecision(17)<<mask<<" "<<r.mu<<" "<<r.sigma<<" "<<r.nu<<" "<<r.logLik<<" "<<int(r.status)<<" "<<std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-t).count()/20<<"\n";}}
