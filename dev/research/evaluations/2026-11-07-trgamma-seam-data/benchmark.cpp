#if defined(UMVL_BASELINE)
#include "baseline-invtrgamma.hpp"
#else
#include <cppunivariate/distributions/invtrgamma/mle.hpp>
#endif
#include <chrono>
#include <iostream>
#include <iomanip>
int main() {
  std::vector<double>x,y;double v;
  while(std::cin>>v){x.push_back(v);y.push_back(1/v);}
  for(int inverse=0;inverse<2;++inverse)for(int held=0;held<2;++held) {
    const auto start=std::chrono::steady_clock::now();
    double a=0,b=0,s=0,ll=0;int status=0;
    for(int j=0;j<20;++j) {
      if(inverse) {
        const auto r=held?cppunivariate::invtrgamma_mle_fixed_scale({y.data(),y.size()},.5):cppunivariate::invtrgamma_mle({y.data(),y.size()});
        a=r.shape1;b=r.shape2;s=r.scale;ll=r.logLik;status=static_cast<int>(r.status);
      } else {
        const auto r=held?cppunivariate::trgamma_mle_fixed_scale({x.data(),x.size()},2):cppunivariate::trgamma_mle({x.data(),x.size()});
        a=r.shape1;b=r.shape2;s=r.scale;ll=r.logLik;status=static_cast<int>(r.status);
      }
    }
    std::cout<<std::setprecision(17)<<inverse<<" "<<held<<" "<<status<<" "<<a<<" "<<b<<" "<<s<<" "<<ll<<" "<<std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count()/20<<"\n";
  }
}
