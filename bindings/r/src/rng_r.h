// R RNG adapter for the core's duck-typed sampler interface.
// Rcpp exports establish RNGScope before any of these primitives are called.
#pragma once
#include <Rcpp.h>
namespace dist_impl {
struct RRng {
  double runif(double a, double b) { return a + (b - a) * R::unif_rand(); }
  double rnorm() { return R::norm_rand(); }
  double rgamma(double shape, double scale = 1.0) { return R::rgamma(shape, scale); }
  double rexp(double scale) { return R::rexp(scale); }
  double rt(double df) { return R::rt(df); }
  double rpois(double lambda) { return R::rpois(lambda); }
};
}
