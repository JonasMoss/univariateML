// Inverse Burr (Dagum) distribution: Rcpp::NumericVector wrappers. mlinvburr fits
// only (shape1, shape2); scale defaults to 1.
#include "common.h"
#include "rng_r.h"
#include "cppunivariate/distributions/invburr.hpp"

namespace dist_impl {

Rcpp::NumericVector dinvburr(const Rcpp::NumericVector &x, const Rcpp::NumericVector &shape1,
                   const Rcpp::NumericVector &shape2, double scale = 1.0,
                   bool log = false) {
  const std::size_t n = recycle_length(x.size(), shape1.size(), shape2.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::dinvburr(
        x[i % x.size()], shape1[i % shape1.size()],
        shape2[i % shape2.size()], scale, log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector pinvburr(const Rcpp::NumericVector &q, const Rcpp::NumericVector &shape1,
                   const Rcpp::NumericVector &shape2, double scale = 1.0,
                   bool lower_tail = true, bool log_p = false) {
  const std::size_t n = recycle_length(q.size(), shape1.size(), shape2.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::pinvburr(
        q[i % q.size()], shape1[i % shape1.size()],
        shape2[i % shape2.size()], scale, lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector qinvburr(const Rcpp::NumericVector &p, const Rcpp::NumericVector &shape1,
                   const Rcpp::NumericVector &shape2, double scale = 1.0,
                   bool lower_tail = true, bool log_p = false) {
  const std::size_t n = recycle_length(p.size(), shape1.size(), shape2.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::qinvburr(
        p[i % p.size()], shape1[i % shape1.size()],
        shape2[i % shape2.size()], scale, lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector rinvburr(int n, double shape1 = 1.0, double shape2 = 1.0,
                   double scale = 1.0) {
  if (n < 0 || !(shape1 > 0.0 && shape2 > 0.0 && scale > 0.0)) {
    Rcpp::stop("Invalid parameters for rinvburr");
  }
  Rcpp::NumericVector out(static_cast<std::size_t>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = cppunivariate::distributions::rinvburr(rng, shape1, shape2, scale);
  }
  return out;
}

} // namespace dist_impl
