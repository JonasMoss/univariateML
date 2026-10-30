// Scratch timing harness, not part of the library artifact.
// Default-parameter Bhattacharjee sample: mu=0, sigma=1, a=2
// (normal plus independent uniform(-2,2), as in bhatt/escape-study.R model 2).
#include "cppunivariate/distributions/bhatt/mle.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <chrono>
#include <cstdio>
#include <cstdlib>
#include <vector>

int main(int argc, char **argv) {
  const std::size_t n = argc > 1 ? std::strtoul(argv[1], nullptr, 10) : 1000;
  const std::uint64_t seed = argc > 2 ? std::strtoull(argv[2], nullptr, 10) : 20261029ull;
  cppunivariate::distributions::StdRng<> rng(seed);
  std::vector<double> x(n);
  for (std::size_t i = 0; i < n; ++i)
    x[i] = rng.rnorm() + rng.runif(-2, 2);
  const auto t0 = std::chrono::steady_clock::now();
  const auto r = cppunivariate::bhatt_mle(cppunivariate::span{x.data(), x.size()});
  const auto t1 = std::chrono::steady_clock::now();
  const double secs = std::chrono::duration<double>(t1 - t0).count();
  std::printf("n=%zu seed=%llu time=%.6f status=%d iters=%d mu=%.15g sigma=%.15g a=%.15g logLik=%.15g\n",
      n, (unsigned long long)seed, secs, (int)r.status, r.iterations,
      r.mu, r.sigma, r.a, r.logLik);
  return 0;
}
