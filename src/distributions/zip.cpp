// Zero-inflated Poisson distribution: Rcpp::NumericVector wrappers.
#include "common.h"
#include "rng_r.h"
#include "../cppunivariate/distributions/zip.hpp"

namespace dist_impl {

Rcpp::NumericVector dzip(const Rcpp::NumericVector &x, const Rcpp::NumericVector &lambda,
               const Rcpp::NumericVector &pi, bool log = false) {
  const std::size_t n = recycle_length(x.size(), lambda.size(), pi.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::dzip(
        x[i % x.size()], lambda[i % lambda.size()], pi[i % pi.size()], log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector pzip(const Rcpp::NumericVector &q, const Rcpp::NumericVector &lambda,
               const Rcpp::NumericVector &pi, bool lower_tail = true,
               bool log_p = false) {
  const std::size_t n = recycle_length(q.size(), lambda.size(), pi.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::pzip(
        q[i % q.size()], lambda[i % lambda.size()], pi[i % pi.size()],
        lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector qzip(const Rcpp::NumericVector &p, const Rcpp::NumericVector &lambda,
               const Rcpp::NumericVector &pi, bool lower_tail = true,
               bool log_p = false) {
  const std::size_t n = recycle_length(p.size(), lambda.size(), pi.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::qzip(
        p[i % p.size()], lambda[i % lambda.size()], pi[i % pi.size()],
        lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector rzip(int n, double lambda, double pi) {
  if (n < 0 || !(lambda > 0.0) || !(pi >= 0.0 && pi <= 1.0)) {
    Rcpp::stop("Invalid parameters for rzip");
  }
  Rcpp::NumericVector out(static_cast<std::size_t>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = cppunivariate::distributions::rzip(rng, lambda, pi);
  }
  return out;
}

} // namespace dist_impl
