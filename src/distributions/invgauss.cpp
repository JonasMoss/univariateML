// Inverse-Gaussian distribution: Rcpp::NumericVector wrappers.
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/invgauss.hpp"

namespace dist_impl {

Rcpp::NumericVector dinvgauss(const Rcpp::NumericVector &x, const Rcpp::NumericVector &mean,
                    const Rcpp::NumericVector &shape, bool log = false) {
  const std::size_t n = recycle_length(x.size(), mean.size(), shape.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::dinvgauss(
        x[i % x.size()], mean[i % mean.size()], shape[i % shape.size()],
        log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector pinvgauss(const Rcpp::NumericVector &q, const Rcpp::NumericVector &mean,
                    const Rcpp::NumericVector &shape, bool lower_tail = true,
                    bool log_p = false) {
  const std::size_t n = recycle_length(q.size(), mean.size(), shape.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::pinvgauss(
        q[i % q.size()], mean[i % mean.size()], shape[i % shape.size()],
        lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector qinvgauss(const Rcpp::NumericVector &p, const Rcpp::NumericVector &mean,
                    const Rcpp::NumericVector &shape, bool lower_tail = true,
                    bool log_p = false) {
  const std::size_t n = recycle_length(p.size(), mean.size(), shape.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::qinvgauss(
        p[i % p.size()], mean[i % mean.size()], shape[i % shape.size()],
        lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector rinvgauss(int n, double mean = 1.0, double shape = 1.0) {
  if (n < 0 || !(mean > 0.0 && shape > 0.0)) {
    Rcpp::stop("Invalid parameters for rinvgauss");
  }
  Rcpp::NumericVector out(static_cast<std::size_t>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = cppunivariate::distributions::rinvgauss(rng, mean, shape);
  }
  return out;
}

} // namespace dist_impl
