#include "cppunivariate/distributions/gengamma/local.hpp"
#include <array>
#include <iomanip>
#include <iostream>
#include <vector>
int main() {
  double mu,sigma,q;
  int mask;
  const std::vector<double> y{-1.4,-0.3,0.2,0.6,1.1,1.8};
  while (std::cin >> mu >> sigma >> q >> mask) {
    std::array<double,3> p{mu,sigma,q};
    const std::array<bool,3> free{bool(mask&1),bool(mask&2),bool(mask&4)};
    const bool ok=cppunivariate::gengamma_fit_detail::polish_local(
        {y.data(),y.size()},0.,1.,p,free,1e-10,100);
    std::cout << std::setprecision(17) << p[0] << ' ' << p[1] << ' ' << p[2] << ' ' << ok << '\n';
  }
  return 0;
}
