#include "cppunivariate/distributions/emg/mle.hpp"
#include "json_loader.hpp"
#include <iostream>
#include <iomanip>
#include <chrono>
int main() {
  std::cout<<std::unitbuf<<std::setprecision(17);
  const std::vector<double> a{-2,-1,0,1,2},b{0,0,1,2};
  auto print=[](const char* name,const cppunivariate::EmgResult& r,double seconds) {
    std::cout<<name<<','<<static_cast<int>(r.status)<<','<<r.mu<<','<<r.sigma<<','<<r.lambda<<','<<r.logLik<<','<<r.iterations<<','<<r.stop_detail.loglik_lower<<','<<r.stop_detail.loglik_upper<<','<<seconds<<'\n';
  };
  auto start=std::chrono::steady_clock::now();
  print("exp-face",cppunivariate::emg_mle({a.data(),a.size()}),std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count());
  print("normal-face",cppunivariate::emg_mle({a.data(),a.size()},1e-8,500,0,1),0);
  print("endpoint",cppunivariate::emg_mle({b.data(),b.size()},1e-8,500,0,NAN,1),0);
  const auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/emg/golden.json");
  for(int mask=0;mask<8;++mask) {
    start=std::chrono::steady_clock::now();
    const auto r=cppunivariate::emg_mle({g.data.data(),g.data.size()},1e-10,500,mask&1?g.estimates.at("mu"):NAN,mask&2?g.estimates.at("sigma"):NAN,mask&4?g.estimates.at("lambda"):NAN);
    print(std::to_string(mask).c_str(),r,std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count());
  }
}
