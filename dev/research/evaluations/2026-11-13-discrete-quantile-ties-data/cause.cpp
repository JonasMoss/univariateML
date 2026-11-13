#include <cmath>
#include <cstdio>
#include "cppunivariate/distributions/pois/dpqr.hpp"
namespace d = cppunivariate::distributions;
int main() {
  int fail = 0, explained = 0;
  for (double lambda : {0.1, 0.5, 1.0, 2.5, 7.0, 20.0, 55.5, 130.0})
    for (int k = 0; k <= 300; ++k) {
      const double F = d::ppois(k, lambda, true, false);
      if (!(F > 0) || !(F < 0.999) || d::qpois(F, lambda, true, false) == k) continue;
      ++fail;
      const double lt = d::ppois(k, lambda, true, true);   // the log tail the search compares
      if (std::log(F) > lt) ++explained;
    }
  std::printf("failing round trips %d, of which log(p(k)) > log tail at k: %d\n", fail, explained);
}
