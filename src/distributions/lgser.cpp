// Logarithmic-series distribution: Rcpp::NumericVector wrappers.
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/lgser.hpp"

namespace dist_impl {

Rcpp::NumericVector dlgser(const Rcpp::NumericVector &x, const Rcpp::NumericVector &theta,
                 bool log = false) {
  const std::size_t n = recycle_length(x.size(), theta.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::dlgser(
        x[i % x.size()], theta[i % theta.size()], log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector plgser(const Rcpp::NumericVector &q, const Rcpp::NumericVector &theta,
                 bool lower_tail = true, bool log_p = false) {
  const std::size_t n = recycle_length(q.size(), theta.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::plgser(
        q[i % q.size()], theta[i % theta.size()], lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector qlgser(const Rcpp::NumericVector &p, const Rcpp::NumericVector &theta,
                 bool lower_tail = true, bool log_p = false) {
  const std::size_t n = recycle_length(p.size(), theta.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::qlgser(
        p[i % p.size()], theta[i % theta.size()], lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector rlgser(int n, double theta) {
  if (n < 0 || !(theta > 0.0 && theta < 1.0)) {
    Rcpp::stop("Invalid parameters for rlgser");
  }
  Rcpp::NumericVector out(static_cast<std::size_t>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = cppunivariate::distributions::rlgser(rng, theta);
  }
  return out;
}

} // namespace dist_impl
