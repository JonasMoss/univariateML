// Nakagami distribution: Rcpp::NumericVector wrappers around cppunivariate::distributions.
#include "common.h"
#include "rng_r.h"
#include "cppunivariate/distributions/nakagami.hpp"

namespace dist_impl {

Rcpp::NumericVector dnaka(const Rcpp::NumericVector &x, const Rcpp::NumericVector &shape,
                const Rcpp::NumericVector &scale, bool log = false) {
  const std::size_t n = recycle_length(x.size(), shape.size(), scale.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double xi = x[i % x.size()];
    const double k = shape[i % shape.size()];
    const double s = scale[i % scale.size()];
    const double y = cppunivariate::distributions::dnaka(xi, k, s, log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector pnaka(const Rcpp::NumericVector &q, const Rcpp::NumericVector &shape,
                const Rcpp::NumericVector &scale, bool lower_tail = true,
                bool log_p = false) {
  const std::size_t n = recycle_length(q.size(), shape.size(), scale.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double qi = q[i % q.size()];
    const double k = shape[i % shape.size()];
    const double s = scale[i % scale.size()];
    const double y =
        cppunivariate::distributions::pnaka(qi, k, s, lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector qnaka(const Rcpp::NumericVector &p, const Rcpp::NumericVector &shape,
                const Rcpp::NumericVector &scale, bool lower_tail = true,
                bool log_p = false) {
  const std::size_t n = recycle_length(p.size(), shape.size(), scale.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double pi = p[i % p.size()];
    const double k = shape[i % shape.size()];
    const double s = scale[i % scale.size()];
    const double y =
        cppunivariate::distributions::qnaka(pi, k, s, lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector rnaka(int n, double shape, double scale) {
  if (n < 0 || !(shape > 0.0 && scale > 0.0)) {
    Rcpp::stop("Invalid parameters for rnaka");
  }
  Rcpp::NumericVector out(static_cast<std::size_t>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = cppunivariate::distributions::rnaka(rng, shape, scale);
  }
  return out;
}

} // namespace dist_impl
