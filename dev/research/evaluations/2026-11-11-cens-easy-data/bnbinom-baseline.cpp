// Isolate the unchanged baseline alpha-swap test under double intermediates.
#include "cppunivariate/distributions/bnbinom/mle.hpp"
#include <iostream>
#include <iomanip>
#include <vector>
int main() {
  // Same golden sample as commit 206e979f, loaded by the study shell script.
  std::vector<int> x;
  int value;
  while(std::cin>>value)x.push_back(value);
  int failures=0;
  std::cout<<std::setprecision(17);
  for(double held:{1.,5.,1.0672234787479615,5.3647338351087397}) {
    const auto a=cppunivariate::bnbinom_mle({x.data(),x.size()},1e-8,600,held,NAN,NAN);
    const auto b=cppunivariate::bnbinom_mle({x.data(),x.size()},1e-8,600,NAN,NAN,held);
    std::cout<<held<<' '<<a.logLik<<' '<<b.logLik<<' '<<(a.logLik-b.logLik)<<'\n';
    if(a.logLik!=b.logLik)++failures;
  }
  return failures?1:0;
}
