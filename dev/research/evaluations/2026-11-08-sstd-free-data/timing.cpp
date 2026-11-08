#include "cppunivariate/distributions/sstd/mle.hpp"
#include "json_loader.hpp"
#include <chrono>
#include <iostream>
#include <iomanip>
int main(){const auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/sstd/golden.json");
for(int mask=0;mask<16;++mask){auto t=std::chrono::steady_clock::now();auto r=cppunivariate::sstd_mle({g.data.data(),g.data.size()},1.220703e-4,200,mask&1?0:NAN,mask&2?1.4:NAN,mask&4?5:NAN,mask&8?1.4:NAN);std::cout<<std::setprecision(17)<<mask<<" "<<std::chrono::duration<double>(std::chrono::steady_clock::now()-t).count()<<" "<<int(r.status)<<" "<<r.certified<<" "<<r.mu<<" "<<r.sigma<<" "<<r.nu<<" "<<r.xi<<" "<<r.logLik<<std::endl;}}
