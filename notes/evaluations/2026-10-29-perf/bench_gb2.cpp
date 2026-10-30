// Scratch timing harness, not part of the library artifact.
// Mirrors notes/evaluations/2026-10-29-gb2-implementation-report.md:
// gamma-ratio draws with (a,b,p,q)=(2,1,.4,.3), mt19937_64 seed 12345.
#include "cppunivariate/distributions/gb2/mle.hpp"
#include "cppunivariate/distributions/gb2/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <chrono>
#include <cstdio>
#include <cstdlib>
#include <vector>

int main(int argc, char **argv) {
  const std::size_t n = argc > 1 ? std::strtoul(argv[1], nullptr, 10) : 10000;
  const std::uint64_t seed = argc > 2 ? std::strtoull(argv[2], nullptr, 10) : 12345ull;
  cppunivariate::distributions::StdRng<> rng(seed);
  std::vector<double> x(n);
  for (std::size_t i = 0; i < n; ++i)
    x[i] = cppunivariate::distributions::rgb2(rng, 0.3, 2.0, 0.4, 1.0);
  const auto t0 = std::chrono::steady_clock::now();
  const auto r = cppunivariate::gb2_mle(cppunivariate::span{x.data(), x.size()});
  const auto t1 = std::chrono::steady_clock::now();
  const double secs = std::chrono::duration<double>(t1 - t0).count();
  std::printf("n=%zu seed=%llu time=%.6f status=%d evals=%d shape1=%.15g shape2=%.15g shape3=%.15g scale=%.15g logLik=%.15g\n",
      n, (unsigned long long)seed, secs, (int)r.status, r.iterations,
      r.shape1, r.shape2, r.shape3, r.scale, r.logLik);
  return 0;
}
