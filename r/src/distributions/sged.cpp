// Skew GED: Rcpp::NumericVector wrappers around cppunivariate::distributions.
#include "common.h"
#include "rng_r.h"
#include "cppunivariate/distributions/sged.hpp"

namespace dist_impl {

Rcpp::NumericVector dsged(const Rcpp::NumericVector &x, const Rcpp::NumericVector &mean,
                const Rcpp::NumericVector &sd, const Rcpp::NumericVector &nu, const Rcpp::NumericVector &xi,
                bool log = false) {
  const std::size_t n = recycle_length(
      x.size(), mean.size(),
      std::max(sd.size(), std::max(nu.size(), xi.size())));
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double xv = x[i % x.size()];
    const double m = mean[i % mean.size()];
    const double s = sd[i % sd.size()];
    const double v = nu[i % nu.size()];
    const double xi_val = xi[i % xi.size()];
    const double y = cppunivariate::distributions::dsged(xv, m, s, v, xi_val, log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector psged(const Rcpp::NumericVector &q, const Rcpp::NumericVector &mean,
                const Rcpp::NumericVector &sd, const Rcpp::NumericVector &nu,
                const Rcpp::NumericVector &xi) {
  const std::size_t n = recycle_length(
      q.size(), mean.size(),
      std::max(sd.size(), std::max(nu.size(), xi.size())));
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double qv = q[i % q.size()];
    const double m = mean[i % mean.size()];
    const double s = sd[i % sd.size()];
    const double v = nu[i % nu.size()];
    const double xi_val = xi[i % xi.size()];
    const double y = cppunivariate::distributions::psged(qv, m, s, v, xi_val);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector qsged(const Rcpp::NumericVector &p, const Rcpp::NumericVector &mean,
                const Rcpp::NumericVector &sd, const Rcpp::NumericVector &nu,
                const Rcpp::NumericVector &xi) {
  const std::size_t n = recycle_length(
      p.size(), mean.size(),
      std::max(sd.size(), std::max(nu.size(), xi.size())));
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double pv = p[i % p.size()];
    const double m = mean[i % mean.size()];
    const double s = sd[i % sd.size()];
    const double v = nu[i % nu.size()];
    const double xi_val = xi[i % xi.size()];
    const double y = cppunivariate::distributions::qsged(pv, m, s, v, xi_val);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector rsged(int n, double mean = 0.0, double sd = 1.0, double nu = 2.0,
                double xi = 1.5) {
  if (n < 0 || !(sd > 0.0 && nu > 0.0 && xi > 0.0)) {
    Rcpp::stop("Invalid parameters for rsged");
  }
  Rcpp::NumericVector out(static_cast<std::size_t>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = cppunivariate::distributions::rsged(rng, mean, sd, nu, xi);
  }
  return out;
}

} // namespace dist_impl
