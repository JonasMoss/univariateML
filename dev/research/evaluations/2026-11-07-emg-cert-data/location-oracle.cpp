#include "cppunivariate/distributions/emg/mle.hpp"
#include <iostream>
#include <iomanip>
int main() {
  const std::vector<double> x{-2,-1,0,1,2};
  std::cout<<std::setprecision(17)<<"sigma,lambda,mu,lower,upper,ok\n";
  for(double s:{.01,1.,100.})for(double l:{.001,1.,1000.}) {
    const auto r=cppunivariate::emg_detail::location_certificate({x.data(),x.size()},s,l,1e-10,500);
    std::cout<<s<<','<<l<<','<<r.mu<<','<<r.lower<<','<<r.upper<<','<<r.ok<<'\n';
  }
}
