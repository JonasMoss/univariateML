#ifndef FPAR_HEADER
#define FPAR_HEADER "cppunivariate/distributions/fpareto/mle.hpp"
#endif
#include FPAR_HEADER
#include <iostream>
#include <iomanip>
#include <cstdlib>
int main() {
 std::string family,token;int id,n;
 if(!(std::cin>>family>>id>>n))return 1;
 std::vector<double> h(5),x(n);
 for(double&v:h){std::cin>>token;v=std::strtod(token.c_str(),nullptr);}
 for(double&v:x)std::cin>>v;
 const auto print=[&](const auto&r){
  std::cout<<std::setprecision(17)<<id<<' '<<int(r.status)<<' '<<r.certified<<' '<<r.shape1<<' '<<r.shape2<<' '<<r.shape3<<' '<<r.scale<<' '<<r.logLik<<' '<<r.iterations;
#ifndef UMVL_BASELINE
  std::cout<<' '<<r.profile_evaluations;
#else
  std::cout<<" 0";
#endif
  std::cout<<' '<<r.stop_detail.budget_exhausted<<' '<<r.stop_detail.candidates.size();
  for(const auto&c:r.stop_detail.candidates){std::cout<<' '<<c.coef.size();for(double v:c.coef)std::cout<<' '<<v;std::cout<<' '<<c.loglik_lower<<' '<<c.loglik_upper;}
  std::cout<<std::endl;
 };
 if(family=="fpareto")print(cppunivariate::fpareto_mle({x.data(),x.size()},h[0],1e-7,1000,h[1],h[2],h[3],h[4]));
 else print(cppunivariate::gb2_mle({x.data(),x.size()},1e-10,300,h[1],h[2],h[3],h[4]));
}
