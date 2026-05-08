// Standardised Student-t: Rcpp::NumericVector wrappers around cppunivariate::distributions.
#include "common.h"
#include "rng_r.h"
#include "cppunivariate/distributions/std/dpqr.hpp"

namespace dist_impl {

Rcpp::NumericVector dstd(const Rcpp::NumericVector &x, const Rcpp::NumericVector &mean, const Rcpp::NumericVector &sd,
               const Rcpp::NumericVector &nu, bool log = false) {
  const std::size_t n = recycle_length(x.size(), mean.size(),
                                 std::max(sd.size(), nu.size()));
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double xi = x[i % x.size()];
    const double m = mean[i % mean.size()];
    const double s = sd[i % sd.size()];
    const double v = nu[i % nu.size()];
    const double y = cppunivariate::distributions::dstd(xi, m, s, v, log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector pstd(const Rcpp::NumericVector &q, const Rcpp::NumericVector &mean, const Rcpp::NumericVector &sd,
               const Rcpp::NumericVector &nu, bool lower_tail = true,
               bool log_p = false) {
  const std::size_t n = recycle_length(q.size(), mean.size(),
                                 std::max(sd.size(), nu.size()));
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double qi = q[i % q.size()];
    const double m = mean[i % mean.size()];
    const double s = sd[i % sd.size()];
    const double v = nu[i % nu.size()];
    const double y =
        cppunivariate::distributions::pstd(qi, m, s, v, lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector qstd(const Rcpp::NumericVector &p, const Rcpp::NumericVector &mean, const Rcpp::NumericVector &sd,
               const Rcpp::NumericVector &nu, bool lower_tail = true,
               bool log_p = false) {
  const std::size_t n = recycle_length(p.size(), mean.size(),
                                 std::max(sd.size(), nu.size()));
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double pi = p[i % p.size()];
    const double m = mean[i % mean.size()];
    const double s = sd[i % sd.size()];
    const double v = nu[i % nu.size()];
    const double y =
        cppunivariate::distributions::qstd(pi, m, s, v, lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector rstd(int n, double mean = 0.0, double sd = 1.0, double nu = 5.0) {
  if (n < 0 || !(sd > 0.0 && nu > 2.0)) {
    Rcpp::stop("Invalid parameters for rstd");
  }
  Rcpp::NumericVector out(static_cast<std::size_t>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = cppunivariate::distributions::rstd(rng, mean, sd, nu);
  }
  return out;
}

} // namespace dist_impl
