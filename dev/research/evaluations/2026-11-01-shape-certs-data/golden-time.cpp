#include "cppunivariate/distributions/ht/mle.hpp"
#include "cppunivariate/distributions/dgamma/mle.hpp"
#include "cppunivariate/distributions/slash/mle.hpp"
#include "../../../../univariateML-cpp/tests/json_loader.hpp"
#include <chrono>
#include <iostream>
#include <limits>
#include <string>
int main() {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  std::cout<<"family,repetition,n,status,logLik,ms\n";
  for(const std::string family:{"ht","dgamma","slash"}) {
    const auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/"+family+"/golden.json");
    const cppunivariate::span x{g.data.data(),g.data.size()};
    for(int i=0;i<20;++i) {
      int status=-1; double ll=nan; auto start=std::chrono::steady_clock::now();
      if(family=="ht") {auto r=cppunivariate::ht_mle(x,0,nan,nan);status=(int)r.status;ll=r.logLik;}
      else if(family=="dgamma") {auto r=cppunivariate::dgamma_mle(x);status=(int)r.status;ll=r.logLik;}
      else {auto r=cppunivariate::slash_mle(x);status=(int)r.status;ll=r.logLik;}
      auto stop=std::chrono::steady_clock::now();
      std::cout<<family<<','<<i<<','<<g.data.size()<<','<<status<<','<<ll<<','<<std::chrono::duration<double,std::milli>(stop-start).count()<<'\n';
    }
  }
}
