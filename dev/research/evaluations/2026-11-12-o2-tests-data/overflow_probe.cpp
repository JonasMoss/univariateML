#include <cmath>
#include <limits>
// Standalone reduction of the pre-fix branch. Compile with
// g++ -g -O2 -DNDEBUG -std=gnu++17 -S -fdump-tree-optimized overflow_probe.cpp
// and inspect whether the fallback log(a)+log(b) survives the finite guard.
double overflow_log(double a, double b) {
  if (!(a > 0 && std::isfinite(a))) return NAN;
  const double p = a*b;
  return p == std::numeric_limits<double>::infinity()
      ? std::log(a)+std::log(b) : std::log(p);
}
