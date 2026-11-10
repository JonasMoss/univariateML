// Run against the baseline or final headers, -O2 -std=c++17, one CPU slot.
// CSV: hold mask (mu=1,sigma=2,a=4), status, mu,sigma,a,logLik,seconds.
#include "cppunivariate/distributions/bhatt/mle.hpp"
#include "json_loader.hpp"
#include <chrono>
#include <iomanip>
#include <iostream>
int main() {
  const auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/bhatt/golden.json");
  std::cout<<std::setprecision(17);
  for(int mask=0;mask<8;++mask){
    const auto start=std::chrono::steady_clock::now();
    const auto f=cppunivariate::bhatt_mle({g.data.data(),g.data.size()},1e-8,500,
      mask&2?1.057897350270679:NAN,mask&4?1.7336058787117143:NAN,
      mask&1?-0.0038000682197418395:NAN);
    const double elapsed=std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count();
    std::cout<<mask<<','<<static_cast<int>(f.status)<<','<<f.mu<<','<<f.sigma<<','<<f.a<<','<<f.logLik<<','<<elapsed<<'\n';
  }
}
