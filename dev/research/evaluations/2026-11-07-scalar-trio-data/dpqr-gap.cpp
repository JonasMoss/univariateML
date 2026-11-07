#include "cppunivariate/distributions/fatigue/dpqr.hpp"
#include "cppunivariate/distributions/fatigue/mle.hpp"
#include <iostream>
#include <iomanip>
int main() {
  const double beta=1e300;
  const std::vector<double> x{beta,std::nextafter(beta,INFINITY)};
  const auto fit=cppunivariate::fatigue_mle({x.data(),x.size()},1e-10,200,NAN,beta);
  double density_ll=0;
  for(double v:x)density_ll+=cppunivariate::distributions::dfatigue(v,fit.alpha,beta,0,true);
  std::cout<<std::setprecision(17)<<"status "<<int(fit.status)<<" alpha "<<fit.alpha<<" fit_ll "<<fit.logLik<<" dpqr_ll "<<density_ll<<" difference "<<density_ll-fit.logLik<<"\n";
}
