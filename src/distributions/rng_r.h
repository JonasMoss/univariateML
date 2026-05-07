// R-shell RNG adapter: forwards rng.{runif,rnorm,rgamma,rt} to R's RNG
// (R::runif, R::norm_rand, R::rgamma, R::rt) so set.seed() reproducibility
// matches the legacy direct-R::* call sites bit-for-bit.
//
// The portable samplers under src/ml/distributions/ are
// `template <typename Rng> double r<dist>(Rng& rng, ...)`. The R-shell
// wrappers in src/distributions/<dist>.cpp instantiate them with this RRng
// to drive sampling through R's seeded state.

#pragma once

#include <Rcpp.h>

namespace dist_impl {

struct RRng {
  inline double runif(double a, double b) { return R::runif(a, b); }
  inline double rnorm() { return R::norm_rand(); }
  inline double rgamma(double shape, double scale = 1.0) {
    return R::rgamma(shape, scale);
  }
  inline double rt(double df) { return R::rt(df); }
};

} // namespace dist_impl
