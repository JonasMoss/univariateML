#include "cppunivariate/distributions/gengamma/shape_score.hpp"
#include <iomanip>
#include <iostream>
#include <vector>
int main() {
  double q;
  const std::vector<double> y{-1.4,-0.3,0.2,0.6,1.1,1.8};
  while (std::cin >> q) {
    const auto s=cppunivariate::gengamma_fit_detail::shape_score({y.data(),y.size()},0.2,1.3,q);
    std::cout << std::setprecision(17) << s.value << ' ' << s.valid << '\n';
  }
  return 0;
}
