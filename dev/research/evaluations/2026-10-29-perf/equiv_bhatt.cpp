// Old-vs-new equivalence driver for bhatt_mle: many seeded samples, default
// (interior normal+uniform) parameters and samples designed to land on the
// normal and uniform limit faces. Build against orig_include, then the
// edited tree, and diff the two logs.
#include "cppunivariate/distributions/bhatt/mle.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cstdio>
#include <vector>

static void run_case(const char *label, const std::vector<double> &x) {
  const auto r = cppunivariate::bhatt_mle(cppunivariate::span{
      const_cast<double*>(x.data()), x.size()});
  std::printf("%s n=%zu status=%d mu=%.17g sigma=%.17g a=%.17g logLik=%.17g iters=%d\n",
      label, x.size(), (int)r.status, r.mu, r.sigma, r.a, r.logLik, r.iterations);
}

int main() {
  // Default/interior parameter samples (normal + uniform(-2,2)), many seeds.
  for (std::uint64_t seed = 1; seed <= 20; ++seed) {
    for (std::size_t n : {std::size_t(30), std::size_t(100), std::size_t(300)}) {
      cppunivariate::distributions::StdRng<> rng(seed * 1000 + n);
      std::vector<double> x(n);
      for (double &v : x) v = rng.rnorm() + rng.runif(-2, 2);
      char label[64];
      std::snprintf(label, sizeof(label), "interior seed=%llu", (unsigned long long)seed);
      run_case(label, x);
    }
  }
  // Normal-face-ish: pure normal samples (a -> 0 limit).
  for (std::uint64_t seed = 1; seed <= 15; ++seed) {
    cppunivariate::distributions::StdRng<> rng(seed + 5000);
    std::vector<double> x(100);
    for (double &v : x) v = rng.rnorm();
    run_case("normal-face", x);
  }
  // Uniform-face-ish: pure uniform samples (sigma -> 0 limit).
  for (std::uint64_t seed = 1; seed <= 15; ++seed) {
    cppunivariate::distributions::StdRng<> rng(seed + 6000);
    std::vector<double> x(100);
    for (double &v : x) v = rng.runif(-3, 3);
    run_case("uniform-face", x);
  }
  // Intermediate: comparable normal and uniform scale (mix of face/interior).
  for (std::uint64_t seed = 1; seed <= 15; ++seed) {
    cppunivariate::distributions::StdRng<> rng(20261029 + seed);
    std::vector<double> x(20);
    for (double &v : x) v = rng.rnorm() + rng.runif(-2, 2);
    run_case("intermediate-n20", x);
  }
  // Held-parameter subsets, a few seeds.
  for (std::uint64_t seed = 1; seed <= 5; ++seed) {
    cppunivariate::distributions::StdRng<> rng(seed + 9000);
    std::vector<double> x(100);
    for (double &v : x) v = rng.rnorm() + rng.runif(-2, 2);
    const auto r1 = cppunivariate::bhatt_mle(cppunivariate::span{x.data(), x.size()}, 1e-8, 500,
        1.0, std::numeric_limits<double>::quiet_NaN());
    std::printf("held-sigma seed=%llu status=%d mu=%.17g sigma=%.17g a=%.17g logLik=%.17g iters=%d\n",
        (unsigned long long)seed, (int)r1.status, r1.mu, r1.sigma, r1.a, r1.logLik, r1.iterations);
    const auto r2 = cppunivariate::bhatt_mle(cppunivariate::span{x.data(), x.size()}, 1e-8, 500,
        std::numeric_limits<double>::quiet_NaN(), 2.0);
    std::printf("held-a seed=%llu status=%d mu=%.17g sigma=%.17g a=%.17g logLik=%.17g iters=%d\n",
        (unsigned long long)seed, (int)r2.status, r2.mu, r2.sigma, r2.a, r2.logLik, r2.iterations);
  }
  // Degenerate: constant sample (unbounded), tiny n.
  for (std::uint64_t seed = 1; seed <= 5; ++seed) {
    std::vector<double> x(5, 3.0);
    run_case("constant", x);
    cppunivariate::distributions::StdRng<> rng(seed + 7000);
    std::vector<double> y(6);
    for (double &v : y) v = rng.rnorm() + rng.runif(-2, 2);
    run_case("tiny-n6", y);
  }
  return 0;
}
