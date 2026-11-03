// Research driver, not a certified estimator. One sample per input line.
#include "cppunivariate/distributions/burr/mle.hpp"
#include <iostream>
#include <iomanip>
#include <vector>
int main() {
  std::cout << std::setprecision(17);
  std::size_t n;
  while (std::cin >> n) {
    std::vector<double> x(n);
    for (double &v : x) if (!(std::cin >> v)) return 2;
    const auto r = cppunivariate::burr_mle(
        cppunivariate::span{x.data(), x.size()}, 1e-8, 1000);
    std::cout << static_cast<int>(r.status) << ' ' << r.shape1 << ' '
              << r.shape2 << ' ' << r.scale << ' ' << r.logLik << '\n';
  }
}
