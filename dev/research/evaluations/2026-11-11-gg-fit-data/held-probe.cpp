#include "cppunivariate/distributions/gengamma/held_joint.hpp"
#include <iomanip>
#include <iostream>
#include <vector>
int main() {
  double q;
  const std::vector<double> y{-1.4,-0.3,0.2,0.6,1.1,1.8};
  const cppunivariate::span data{y.data(),y.size()};
  while (std::cin >> q) {
    const auto a=cppunivariate::gengamma_fit_detail::location(data,1.3,q);
    const auto b=cppunivariate::gengamma_fit_detail::scale(data,0.2,q,1e-11,100);
    const auto c=cppunivariate::gengamma_fit_detail::location_scale(data,q,1e-11,100);
    std::cout << std::setprecision(17) << a.value << ' ' << int(a.status) << ' '
      << b.value << ' ' << int(b.status) << ' ' << c.mu << ' ' << c.sigma << ' '
      << int(c.status) << '\n';
  }
  return 0;
}
