// R-host specialization of the core sampler template, preserving the frozen
// R EMG stream. R's exponential primitive consumes different uniforms from
// inverse transformation. Even round-tripping exp_rand through a uniform
// perturbs the frozen finite-difference fit, so retain the original arithmetic.
#pragma once
#include "rng_r.h"
#include "cppunivariate/distributions/emg/dpqr.hpp"
namespace cppunivariate {
namespace distributions {
template <>
inline double remg<dist_impl::RRng>(dist_impl::RRng &rng, double mu,
                                  double sigma, double lambda) {
  if (!emg_detail::valid(mu, sigma, lambda)) return special::kNaN;
  const double z = rng.rnorm();
  const double e = rng.rexp(1.0 / lambda);
  return mu + sigma * z + e;
}
}
}
