// Reproducer for a settled dpqr.hpp issue, deliberately left outside this fix.
#include "cppunivariate/special/special.hpp"
#include "cppunivariate/distributions/gev/dpqr.hpp"
#include <iostream>
#include <iomanip>
int main() {
  const double mu=1e8,sigma=1e-12;
  const std::vector<double> x{mu,mu,mu};
  const auto fit=cppunivariate::gev_mle({x.data(),x.size()},1e-10,100,-1,mu,sigma);
  double density_ll=0;for(double v:x)density_ll+=cppunivariate::distributions::dgev(v,mu,sigma,-1,true);
  std::cout<<std::setprecision(17)<<"{\"status\":"<<int(fit.status)
    <<",\"mu\":"<<mu<<",\"sigma\":"<<sigma
    <<",\"support_residual\":"<<cppunivariate::special::support_residual(mu,mu,sigma,-1)
    <<",\"rounded_endpoint_equals_observation\":"<<(mu+sigma==mu?"true":"false")
    <<",\"raw_loglik\":"<<fit.logLik<<",\"settled_density_loglik\":"<<density_ll<<"}\n";
  return fit.status==cppunivariate::GevStatus::Ok && std::abs(density_ll-fit.logLik-3)<1e-10?0:1;
}
