#include "cppunivariate/distributions/emg/mle.hpp"
#include "json_loader.hpp"
#include <chrono>
#include <iostream>
#include <iomanip>
int main(int argc,char**) {
  const double tol=argc>1?1e-8:1e-10;
  const auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/emg/golden.json");
  std::cout<<std::setprecision(17)<<"mask,status,mu,sigma,lambda,logLik,seconds\n";
  for(int mask=0;mask<8;++mask) {
    const auto start=std::chrono::steady_clock::now();
    const auto r=cppunivariate::emg_mle({g.data.data(),g.data.size()},tol,500,
      mask&1?g.estimates.at("mu"):NAN,mask&2?g.estimates.at("sigma"):NAN,
      mask&4?g.estimates.at("lambda"):NAN);
    std::cout<<mask<<','<<static_cast<int>(r.status)<<','<<r.mu<<','<<r.sigma<<','<<r.lambda<<','<<r.logLik<<','
      <<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<'\n';
  }
}
