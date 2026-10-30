// Old-vs-new equivalence driver for gb2_mle: many seeded samples, both
// default (interior) parameters and samples designed to land near/at each
// documented limit face. Builds against whichever cppunivariate headers are
// on the include path -- run once against orig_include, once against the
// edited tree, and diff the two logs.
#include "cppunivariate/distributions/gb2/mle.hpp"
#include "cppunivariate/distributions/gb2/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cstdio>
#include <vector>

static void run_case(const char *label, const std::vector<double> &x) {
  const auto r = cppunivariate::gb2_mle(cppunivariate::span{
      const_cast<double*>(x.data()), x.size()});
  std::printf("%s n=%zu status=%d shape1=%.17g shape2=%.17g shape3=%.17g scale=%.17g logLik=%.17g evals=%d\n",
      label, x.size(), (int)r.status, r.shape1, r.shape2, r.shape3, r.scale, r.logLik, r.iterations);
}

int main() {
  // Default/interior parameter samples, many seeds, two sample sizes.
  for (std::uint64_t seed = 1; seed <= 20; ++seed) {
    for (std::size_t n : {std::size_t(200), std::size_t(2000)}) {
      cppunivariate::distributions::StdRng<> rng(seed * 1000 + n);
      std::vector<double> x(n);
      for (std::size_t i = 0; i < n; ++i)
        x[i] = cppunivariate::distributions::rgb2(rng, 0.3, 2.0, 0.4, 1.0);
      char label[64];
      std::snprintf(label, sizeof(label), "interior seed=%llu", (unsigned long long)seed);
      run_case(label, x);
    }
  }
  // Lognormal-boundary-ish: shape1,shape3 both large (both betas -> normal
  // on log scale), should land on or near the lognormal face.
  for (std::uint64_t seed = 1; seed <= 10; ++seed) {
    cppunivariate::distributions::StdRng<> rng(seed + 5000);
    std::vector<double> x(500);
    for (double &v : x) v = std::exp(rng.rnorm());
    run_case("lognormal-face", x);
  }
  // Pareto-boundary-ish: Burr with a small shape1 (q->small, heavy tail)
  // and p held effectively at the power/Pareto edge via a one-sided Burr.
  for (std::uint64_t seed = 1; seed <= 10; ++seed) {
    cppunivariate::distributions::StdRng<> rng(seed + 6000);
    std::vector<double> x(500);
    for (double &v : x) v = std::exp(cppunivariate::distributions::rgb2(rng, 1.5, 1.0, 1e6, 1.0));
    run_case("pareto-face-ish", x);
  }
  // Double-Pareto-ish: both shapes small -> heavy both tails on log scale.
  for (std::uint64_t seed = 1; seed <= 10; ++seed) {
    cppunivariate::distributions::StdRng<> rng(seed + 7000);
    std::vector<double> x(500);
    for (double &v : x) v = cppunivariate::distributions::rgb2(rng, 0.05, 1.0, 0.05, 1.0);
    run_case("double-pareto-ish", x);
  }
  // Tiny/degenerate samples (budget-exhausted / small-n edge behaviour).
  for (std::uint64_t seed = 1; seed <= 10; ++seed) {
    cppunivariate::distributions::StdRng<> rng(seed + 8000);
    std::vector<double> x(8);
    for (double &v : x) v = cppunivariate::distributions::rgb2(rng, 0.3, 2.0, 0.4, 1.0);
    run_case("tiny-n", x);
  }
  // Fixed-parameter subsets (p held, q held, a held, b held), a few seeds.
  for (std::uint64_t seed = 1; seed <= 5; ++seed) {
    cppunivariate::distributions::StdRng<> rng(seed + 9000);
    std::vector<double> x(500);
    for (double &v : x) v = cppunivariate::distributions::rgb2(rng, 0.3, 2.0, 0.4, 1.0);
    const auto r1 = cppunivariate::gb2_mle(cppunivariate::span{x.data(), x.size()}, 1e-7, 300,
        std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(), 0.4, 1.0);
    std::printf("fixed-p-b seed=%llu status=%d shape1=%.17g shape2=%.17g shape3=%.17g scale=%.17g logLik=%.17g evals=%d\n",
        (unsigned long long)seed, (int)r1.status, r1.shape1, r1.shape2, r1.shape3, r1.scale, r1.logLik, r1.iterations);
    const auto r2 = cppunivariate::gb2_mle(cppunivariate::span{x.data(), x.size()}, 1e-7, 300,
        0.3, std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN());
    std::printf("fixed-q seed=%llu status=%d shape1=%.17g shape2=%.17g shape3=%.17g scale=%.17g logLik=%.17g evals=%d\n",
        (unsigned long long)seed, (int)r2.status, r2.shape1, r2.shape2, r2.shape3, r2.scale, r2.logLik, r2.iterations);
  }
  return 0;
}
