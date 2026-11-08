// Diagnostic of a pre-existing CDF transformation outside the quantile search.
// The stable reference is Phi((x/scale)-(mean/scale)+1/scale), expressed with erfc.
#include "cppunivariate/distributions/dnorm/dpqr.hpp"
#include <cmath>
#include <iomanip>
#include <iostream>
int main() {
  const double mean=1e308, scale=1e308, p=.01;
  const double h=cppunivariate::distributions::qdnorm(p,mean,scale);
  const double before=std::nextafter(h,-INFINITY);
  const auto reference=[&](double x) {
    return std::erfc((mean/scale-x/scale-1/scale)/std::sqrt(2.0))/2;
  };
  std::cout<<std::setprecision(17)
      <<"h="<<h<<"\nprevious="<<before
      <<"\ncomputed F(h)="<<cppunivariate::distributions::pdnorm(h,mean,scale)
      <<"\ncomputed F(previous)="<<cppunivariate::distributions::pdnorm(before,mean,scale)
      <<"\nstable reference F(h)="<<reference(h)
      <<"\nstable reference F(previous)="<<reference(before)<<"\n";
}
