#include "cppunivariate/distributions/genbeta/mle.hpp"
#include <iostream>
#include <iomanip>
int main() {
  std::size_t n=0;std::cin>>n;std::vector<double> x(n);
  for(double& v:x)std::cin>>v;
  std::cout<<std::setprecision(17);
  for(double p:{.1,.4,.5,2.,10.}) {
    const auto r=cppunivariate::genbeta_mle({x.data(),x.size()},1,1e-7,1000,p);
    std::cout<<p<<" "<<r.shape2<<" "<<r.shape3<<" "<<r.logLik<<" "<<int(r.status)<<" "<<r.iterations<<" "<<int(r.certificate_failure)<<"\n";
  }
}
