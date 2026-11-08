#include "cppunivariate/distributions/ged/mle.hpp"
#include <iostream>
#include <iomanip>
#include <chrono>
int main(){std::vector<double>x;double v;while(std::cin>>v)x.push_back(v);for(int mask=0;mask<4;++mask){auto t=std::chrono::steady_clock::now();auto r=cppunivariate::ged_mle({x.data(),x.size()},1.220703e-4,200,mask&1?-.017698887677594307:NAN,mask&2?1.0178559902092952:NAN,NAN);std::cout<<std::setprecision(17)<<mask<<" status "<<int(r.status)<<" cert "<<r.certified<<" coef "<<r.mu<<" "<<r.sigma<<" "<<r.nu<<" ll "<<r.logLik<<" work "<<r.iterations<<" budget "<<r.stop_detail.budget_exhausted<<" ms "<<std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-t).count()<<std::endl;}}
