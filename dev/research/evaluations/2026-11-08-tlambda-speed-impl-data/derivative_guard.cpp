#include "cppunivariate/distributions/tlambda/mle.hpp"
#include <iostream>
int main(){
 const std::vector<double>x{0};
 const auto v=cppunivariate::tlambda_fit_detail::value(x,1,0,1024,1000);
 const auto r=cppunivariate::tlambda_fit_detail::inner(x,1,0,1024,1e-10,1000);
 if(!std::isfinite(v.ll) || v.ok || r.ok || r.it!=0)return 1;
 std::cout<<"PASS: finite likelihood "<<v.ll<<"; nonfinite derivatives rejected before damping, iterations="<<r.it<<'\n';
}
