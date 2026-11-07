#include "cppunivariate/distributions/sged/mle.hpp"
#include <chrono>
#include <iomanip>
#include <iostream>
int main() {
  std::cout<<std::setprecision(17);
  const std::vector<double> x{-3,-1,-.5,0,.1,.2,.4,1,2,4};
  const cppunivariate::span data{x.data(),x.size()};
  for(int mask:{1,2,3})for(int route:{0,1}) {
    const double mean=mask&1?.2:NAN,sd=mask&2?1.4:NAN;
    const auto start=std::chrono::steady_clock::now();
    double ll=0;int status=0;
    for(int i=0;i<5;++i) {
      if(route) {
        const auto r=cppunivariate::sged_mle(data,1e-6,200,mean,sd,2);
        ll=r.logLik;status=static_cast<int>(r.status);
      } else {
        const auto r=cppunivariate::skew_hold_detail::fit(data,false,mean,sd,2,NAN,1e-6,200);
        ll=-r.f;status=r.status;
      }
    }
    std::cout<<mask<<' '<<(route?"after":"before")<<' '<<status<<' '<<ll<<' '
             <<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()/5<<'\n';
  }
}
