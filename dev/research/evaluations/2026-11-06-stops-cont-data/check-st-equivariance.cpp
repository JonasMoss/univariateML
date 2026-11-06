#include "cppunivariate/distributions/st/mle.hpp"
#include <iomanip>
#include <iostream>
int main() {
  std::vector<double> x;double value;
  while(std::cin>>value)x.push_back(value);
  std::cout<<std::setprecision(17);
  for(double b:{1.,1e-4,1e4}) {
    const double a=b==1?0:3*b;
    std::vector<double> y;for(double v:x)y.push_back(a+b*v);
    const auto r=cppunivariate::st_mle({y.data(),y.size()});
    std::cout<<b<<" "<<static_cast<int>(r.status)<<" "<<(r.xi-a)/b<<" "<<r.omega/b<<" "<<r.alpha<<" "<<r.nu<<" "<<r.logLik+y.size()*std::log(b)<<"\n";
  }
}
