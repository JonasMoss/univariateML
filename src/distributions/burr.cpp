// Burr (Type XII) distribution: Rcpp::NumericVector wrappers. mlburr fits only
// (shape1, shape2); the scale defaults to 1 (matches actuar::dburr's
// rate=1 -> scale=1 default path).
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/burr.hpp"

namespace dist_impl {

Rcpp::NumericVector dburr(const Rcpp::NumericVector &x, const Rcpp::NumericVector &shape1,
                const Rcpp::NumericVector &shape2, double scale = 1.0,
                bool log = false) {
  const std::size_t n = recycle_length(x.size(), shape1.size(), shape2.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::dburr(
        x[i % x.size()], shape1[i % shape1.size()],
        shape2[i % shape2.size()], scale, log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector pburr(const Rcpp::NumericVector &q, const Rcpp::NumericVector &shape1,
                const Rcpp::NumericVector &shape2, double scale = 1.0,
                bool lower_tail = true, bool log_p = false) {
  const std::size_t n = recycle_length(q.size(), shape1.size(), shape2.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::pburr(
        q[i % q.size()], shape1[i % shape1.size()],
        shape2[i % shape2.size()], scale, lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector qburr(const Rcpp::NumericVector &p, const Rcpp::NumericVector &shape1,
                const Rcpp::NumericVector &shape2, double scale = 1.0,
                bool lower_tail = true, bool log_p = false) {
  const std::size_t n = recycle_length(p.size(), shape1.size(), shape2.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::qburr(
        p[i % p.size()], shape1[i % shape1.size()],
        shape2[i % shape2.size()], scale, lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector rburr(int n, double shape1 = 1.0, double shape2 = 1.0,
                double scale = 1.0) {
  if (n < 0 || !(shape1 > 0.0 && shape2 > 0.0 && scale > 0.0)) {
    Rcpp::stop("Invalid parameters for rburr");
  }
  Rcpp::NumericVector out(static_cast<std::size_t>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = cppunivariate::distributions::rburr(rng, shape1, shape2, scale);
  }
  return out;
}

} // namespace dist_impl
