#include "cppunivariate/distributions/gengamma/profile_root.hpp"
#include <iomanip>
#include <iostream>
#include <vector>
int main() {
  double perturbation;
  while (std::cin >> perturbation) {
    const std::vector<double> y{2.,1.,0.,-1.,-(2.+perturbation)};
    cppunivariate::trgamma_detail::LogData d;
    if (!cppunivariate::trgamma_detail::standardize({y.data(),y.size()},d)) return 1;
    const auto r=cppunivariate::gengamma_fit_detail::profile_root(d,1e-12,1.,1e-10,100);
    std::cout << std::setprecision(17) << r.mu << ' ' << r.sigma << ' ' << r.q << ' ' << r.ok << '\n';
  }
  return 0;
}
