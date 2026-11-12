#ifndef FPAR_HEADER
#define FPAR_HEADER "cppunivariate/distributions/fpareto/mle.hpp"
#endif
#include FPAR_HEADER
#include <iostream>
#include <iomanip>
int main() {
  int family,n;if(!(std::cin>>family>>n))return 1;
  std::vector<double>x(n);for(double&v:x)std::cin>>v;
  const auto print=[](const auto&r){std::cout<<std::setprecision(17)<<int(r.status)<<' '<<r.certified<<' '<<r.shape1<<' '<<r.shape2<<' '<<r.shape3<<' '<<r.scale<<' '<<r.logLik<<std::endl;};
  if(family==0)print(cppunivariate::fpareto_mle(cppunivariate::span{x.data(),x.size()},0,1e-7,1000,.3,NAN,.4,NAN));
  else print(cppunivariate::gb2_mle(cppunivariate::span{x.data(),x.size()}));
}
