#include "cppunivariate/distributions/betapr/mle.hpp"
#include <iostream>
#include <iomanip>
#include <vector>
int main(){const std::vector<double>x{.2,.5,1.,2.,4.};int mode;double a,b;std::cout<<std::setprecision(17);
 while(std::cin>>mode>>a>>b){const double held_a=mode==1?a:NAN,held_b=mode==2?b:NAN;
  cppunivariate::BetaPrimeResult bad{NAN,NAN,1.,NAN,0,cppunivariate::BetaPrimeStatus::BfgsFailure};bad.stop_detail.loglik_lower=-INFINITY;
  const auto before=cppunivariate::betapr_detail::fallback(bad,cppunivariate::span{x.data(),x.size()},held_a,held_b,1.,10.,1000,a*(1+1e-5),b*(1-1e-5),1.);
  const auto after=cppunivariate::betapr_detail::accurate_fallback(before,cppunivariate::span{x.data(),x.size()},held_a,held_b,1.,1e-10,1000);
  std::cout<<static_cast<int>(before.status)<<' '<<before.certified<<' '<<before.alpha<<' '<<before.beta<<' '<<before.scale<<' '<<static_cast<int>(after.status)<<' '<<after.certified<<' '<<after.alpha<<' '<<after.beta<<' '<<after.scale<<'\n';}}
