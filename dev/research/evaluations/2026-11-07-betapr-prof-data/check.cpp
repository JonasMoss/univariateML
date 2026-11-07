#include "cppunivariate/distributions/betapr/mle.hpp"
#include <chrono>
#include <iomanip>
#include <iostream>
int main(){
  std::vector<double>x{.03,.1,.4,.7,1,2,4,12};
  std::cout<<std::setprecision(17);
  for(double a:{.7,2.,5.}){
    auto t=std::chrono::steady_clock::now();
    cppunivariate::BetaPrimeResult r{};
    for(int j=0;j<30;++j)r=cppunivariate::betapr_mle_holds({x.data(),x.size()},a,NAN,NAN);
    std::cout<<a<<" "<<r.beta<<" "<<r.scale<<" "<<r.logLik<<" "<<int(r.status)<<" "
      <<std::chrono::duration<double>(std::chrono::steady_clock::now()-t).count()/30<<"\n";
  }
}
