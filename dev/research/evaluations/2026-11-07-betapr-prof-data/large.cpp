#include "cppunivariate/distributions/betapr/mle.hpp"
#include <chrono>
#include <iomanip>
#include <iostream>
int main(){
  const std::vector<double> base{.03,.1,.4,.7,1,2,4,12};
  std::vector<double> x;x.reserve(100000);
  for(int j=0;j<12500;++j)for(double v:base)x.push_back(v);
  const auto start=std::chrono::steady_clock::now();
  const auto r=cppunivariate::betapr_mle_holds({x.data(),x.size()},2,NAN,NAN);
  std::cout<<std::setprecision(17)<<x.size()<<" "<<r.beta<<" "<<r.scale<<" "<<r.logLik<<" "
    <<int(r.status)<<" "<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\n";
}
