#include "cppunivariate/distributions/sstd/mle.hpp"
#include "json_loader.hpp"
#include <chrono>
#include <iomanip>
#include <iostream>
int main() {
  const auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/sstd/golden.json");
  const cppunivariate::span x{g.data.data(),g.data.size()};
  for(int mode=0;mode<3;++mode) {
    const int repeats=mode==1 ? 100 : 3;
    const auto start=std::chrono::steady_clock::now();
    cppunivariate::SstdResult r{};
    for(int i=0;i<repeats;++i) r=mode==0 ? cppunivariate::sstd_mle(x,1.220703e-4,200) :
      mode==1 ? cppunivariate::sstd_mle(x,1.220703e-4,200,0,NAN,5,1) :
      cppunivariate::sstd_mle(x,1.220703e-4,200,NAN,NAN,NAN,1);
    std::cout<<std::setprecision(17)<<mode<<" "<<repeats<<" "
      <<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()/repeats
      <<" "<<int(r.status)<<" "<<r.mu<<" "<<r.sigma<<" "<<r.nu<<" "<<r.xi<<" "<<r.logLik<<"\n";
  }
}
