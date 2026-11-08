// Standalone numerical evidence; run run-probe.sh through the CPU slot wrapper.
#include "cppunivariate/distributions/dnorm/dpqr.hpp"
#include "cppunivariate/distributions/dlaplace/dpqr.hpp"
#include <iomanip>
#include <iostream>
int main() {
  const double x=-7.976931348623157e307, m=1e308, s=1e308;
  const double h=cppunivariate::distributions::qdnorm(.01,m,s);
  std::cout << std::setprecision(17)
      << "Original standardized point: " << (std::floor(x)+1-m)/s << '\n'
      << "Original standardized predecessor: " << (std::floor(std::nextafter(x,-INFINITY))+1-m)/s << '\n'
      << "Original CDF at predecessor: "
      << cppunivariate::special::fs_normal_p((std::floor(std::nextafter(x,-INFINITY))+1-m)/s,true,false) << '\n'
      << "Corrected CDF at reported point: " << cppunivariate::distributions::pdnorm(x,m,s) << '\n'
      << "Corrected .01 quantile: " << h << '\n'
      << "CDF at quantile: " << cppunivariate::distributions::pdnorm(h,m,s) << '\n'
      << "CDF at previous double: " << cppunivariate::distributions::pdnorm(std::nextafter(h,-INFINITY),m,s) << '\n'
      << "Normal log cell mass at x=-1e308, mean=sd=1e308: "
      << cppunivariate::distributions::ddnorm(-1e308,m,s,true) << '\n'
      << "Laplace log lower tail at x=-1e308, location=1e308, scale=nextafter(1,0): "
      << cppunivariate::distributions::pdlaplace(-1e308,m,std::nextafter(1.,0.),true,true) << '\n';
}
