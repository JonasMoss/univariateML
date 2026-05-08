// Discrete uniform distribution: Rcpp::NumericVector wrappers.
#include "common.h"
#include "rng_r.h"
#include "cppunivariate/distributions/dunif/dpqr.hpp"

namespace dist_impl {

Rcpp::NumericVector ddunif(const Rcpp::NumericVector &x, const Rcpp::NumericVector &min,
                 const Rcpp::NumericVector &max, bool log = false) {
  const std::size_t n = recycle_length(x.size(), min.size(), max.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::ddunif(
        x[i % x.size()], min[i % min.size()], max[i % max.size()], log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector pdunif(const Rcpp::NumericVector &q, const Rcpp::NumericVector &min,
                 const Rcpp::NumericVector &max, bool lower_tail = true,
                 bool log_p = false) {
  const std::size_t n = recycle_length(q.size(), min.size(), max.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::pdunif(
        q[i % q.size()], min[i % min.size()], max[i % max.size()], lower_tail,
        log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector qdunif(const Rcpp::NumericVector &p, const Rcpp::NumericVector &min,
                 const Rcpp::NumericVector &max, bool lower_tail = true,
                 bool log_p = false) {
  const std::size_t n = recycle_length(p.size(), min.size(), max.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::qdunif(
        p[i % p.size()], min[i % min.size()], max[i % max.size()], lower_tail,
        log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector rdunif(int n, double min, double max) {
  if (n < 0 || min > max || min != std::floor(min) ||
      max != std::floor(max)) {
    Rcpp::stop("Invalid parameters for rdunif");
  }
  Rcpp::NumericVector out(static_cast<std::size_t>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = cppunivariate::distributions::rdunif(rng, min, max);
  }
  return out;
}

} // namespace dist_impl
