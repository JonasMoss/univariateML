#include "cppunivariate/distributions/lgamma/mle.hpp"
#include <iomanip>
#include <iostream>

int main() {
  int anchor_power, count, ulps, position, mask;
  while (std::cin >> anchor_power >> count >> ulps >> position >> mask) {
    std::vector<double> data(count + 1, std::ldexp(1., anchor_power));
    data[position ? count : 0] = 1 + std::ldexp(double(ulps), -52);
    const auto fit = cppunivariate::lgamma_mle(
        {data.data(), data.size()}, 1e-10, 200, mask & 1 ? 2. : NAN, mask & 2 ? 3. : NAN);
    const auto result = cppunivariate::outcome(fit);
    std::cout << int(fit.status) << ' ' << result.certified << ' ' << result.unique
              << std::setprecision(17) << ' ' << fit.shapelog << ' ' << fit.ratelog
              << ' ' << fit.logLik << '\n';
  }
}
