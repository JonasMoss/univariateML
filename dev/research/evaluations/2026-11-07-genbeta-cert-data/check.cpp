// Standalone numeric fixture emitter. Independent high-precision check: check.py.
#include "cppunivariate/distributions/genbeta/mle.hpp"
#include <iomanip>
#include <iostream>
int main() {
  const std::vector<double> x{.02,.2,.6,.9};
  std::cout<<std::setprecision(17);
  for(double p:{.1,.5,1.,2.,10.})for(double q:{1e-300,1e-100,1e-16,.01,.7,2.,30.}) {
    const auto r=cppunivariate::genbeta_mle({x.data(),x.size()},1,1e-7,1000,p,q);
    std::cout<<p<<" "<<q<<" "<<r.shape3<<" "<<int(r.status)<<"\n";
  }
  const std::vector<double> singleton{.5};
  const double q=std::exp(705.);
  const auto r=cppunivariate::genbeta_mle({singleton.data(),singleton.size()},1,1e-7,1000,2,q);
  std::cout<<2<<" "<<q<<" "<<r.shape3<<" "<<int(r.status)<<"\n";
}
