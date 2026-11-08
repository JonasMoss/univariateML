#define DOCTEST_CONFIG_DISABLE
#include "test_support.hpp"
#include "cppunivariate/distributions/betapr/mle.hpp"
#include <iostream>
int main(){
 const auto golden=mltest::load_distribution_golden("betapr");
 for(const auto& x:std::vector<std::vector<double>>{{.03,.1,.4,.7,1,2,4,12},golden.data}){
  const auto start=std::chrono::steady_clock::now();cppunivariate::BetaPrimeResult r{};
  for(int i=0;i<10;++i)r=cppunivariate::betapr_mle(mltest::as_span(x));
  std::cout<<std::setprecision(17)<<x.size()<<" "<<std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count()/10<<" "<<r.alpha<<" "<<r.beta<<" "<<r.scale<<" "<<r.logLik<<" "<<static_cast<int>(r.status)<<"\n";
 }
}
