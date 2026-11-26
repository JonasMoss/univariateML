// Replay the fitted coefficients and certification flag for every hold mask.
#include "cppunivariate/distributions/dweibull/mle.hpp"
#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <limits>

int main(int argc, char** argv) {
  if (argc != 5) return 1;
  const double x[] = {std::strtod(argv[1], nullptr), std::strtod(argv[2], nullptr)};
  const auto fit = cppunivariate::dweibull_mle(
      {x, 2}, 1e-10, 200, std::strtod(argv[3], nullptr), std::strtod(argv[4], nullptr));
  std::cout << std::setprecision(std::numeric_limits<double>::max_digits10)
            << fit.shape1 << ' ' << fit.shape2 << ' ' << fit.logLik << ' '
            << static_cast<int>(fit.status) << ' ' << cppunivariate::outcome(fit).certified << '\n';
}
