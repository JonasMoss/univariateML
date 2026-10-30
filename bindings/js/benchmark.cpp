// Same header code, fixture reader and compiler flags as the native doctest.
#include "json_loader.hpp"
#include "cppunivariate/distributions/exp/mle.hpp"
#include "cppunivariate/distributions/gamma/mle.hpp"
#include "cppunivariate/distributions/gb2/mle.hpp"
#include <chrono>
#include <iostream>
#include <iomanip>
int main() {
  std::cout<<std::setprecision(12);
  for(const std::string f : {"exp","gamma","gb2"}) {
    const auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/"+f+"/golden.json");
    cppunivariate::span x{g.data.data(),g.data.size()};
    const int n=f=="gb2"?3:10000;
    volatile double sink=0;
    auto fit=[&] {
      if(f=="exp") {auto r=cppunivariate::exp_mle(x);sink=r.logLik;return std::isfinite(r.logLik);}
      if(f=="gamma") {auto r=cppunivariate::gamma_mle(x,1.220703e-4,100);sink=r.logLik;return r.status==cppunivariate::GammaStatus::Ok;}
      auto r=cppunivariate::gb2_mle(x);sink=r.logLik;return r.status==cppunivariate::Gb2Status::Ok;
    };
    if(!fit())return 1;
    const auto start=std::chrono::steady_clock::now();
    for(int i=0;i<n;++i)if(!fit())return 1;
    const double ms=std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count()/n;
    std::cout<<f<<" "<<n<<" "<<ms<<" "<<sink<<"\n";
  }
}
