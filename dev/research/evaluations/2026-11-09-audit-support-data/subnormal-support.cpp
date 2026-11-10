#include "cppunivariate/special/special.hpp"
#include "cppunivariate/distributions/gev/mle.hpp"
#include <iostream>
#include <iomanip>
int main() {
  const double tiny=std::numeric_limits<double>::denorm_min();
  const std::vector<double> x{-1,0,tiny};
  const cppunivariate::span data{x.data(),x.size()};
  const auto free=cppunivariate::gev_mle(data,1e-10,100,-1,NAN,2);
  const auto held=cppunivariate::gev_mle(data,1e-10,100,-1,-2,2);
  const double residual=cppunivariate::special::support_residual(tiny,-2,2,-1);
  std::cout<<std::setprecision(17)<<tiny<<','<<int(free.status)<<','<<free.mu<<','<<free.sigma
    <<','<<free.logLik<<','<<int(held.status)<<','<<residual<<','<<std::signbit(residual)<<'\n';
}
