// Research timing harness only; no library changes.
#include "cppunivariate/distributions/burr/mle.hpp"
#include <chrono>
#include <iomanip>
#include <iostream>
#include <vector>
int main() {
  std::size_t n;
  std::cout << std::setprecision(17);
  while (std::cin >> n) {
    std::vector<double> x(n);
    for (double &v : x) std::cin >> v;
    cppunivariate::BurrMLE result{};
    const auto start=std::chrono::steady_clock::now();
    for (int i=0;i<10;++i)
      result=cppunivariate::burr_mle(cppunivariate::span{x.data(),x.size()},1e-8,1000);
    const double seconds=std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()/10;
    std::cout << static_cast<int>(result.status) << ' ' << result.shape1 << ' ' << result.shape2 << ' '
              << result.scale << ' ' << result.logLik << ' ' << seconds << '\n';
  }
}
