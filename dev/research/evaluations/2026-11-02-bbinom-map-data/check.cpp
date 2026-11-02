#include <cppunivariate/distributions/bbinom/mle.hpp>
#include <iostream>
#include <iomanip>
#include <vector>
int main() {
  int h, n;
  while (std::cin >> h >> n) {
    std::vector<int> x(h);
    for (int &v : x) std::cin >> v;
    cppunivariate::ispan data{x.data(), x.size()};
    auto r=cppunivariate::bbinom_mle(data,n);
    std::cout << std::setprecision(17) << n << ' ' << static_cast<int>(r.status) << ' ' << r.logLik << ' ' << r.alpha << ' ' << r.beta << '\n';
  }
}
