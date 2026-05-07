// Skew normal: Rcpp::NumericVector wrappers around mlcore::distributions.
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/snorm.hpp"

namespace dist_impl {

Rcpp::NumericVector dsnorm(const Rcpp::NumericVector &x, const Rcpp::NumericVector &mean,
                 const Rcpp::NumericVector &sd, const Rcpp::NumericVector &xi, bool log = false) {
  const std::size_t n = recycle_length(x.size(), mean.size(),
                                 std::max(sd.size(), xi.size()));
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double xv = x[i % x.size()];
    const double m = mean[i % mean.size()];
    const double s = sd[i % sd.size()];
    const double xi_val = xi[i % xi.size()];
    const double y = mlcore::distributions::dsnorm(xv, m, s, xi_val, log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector psnorm(const Rcpp::NumericVector &q, const Rcpp::NumericVector &mean,
                 const Rcpp::NumericVector &sd, const Rcpp::NumericVector &xi) {
  const std::size_t n = recycle_length(q.size(), mean.size(),
                                 std::max(sd.size(), xi.size()));
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double qv = q[i % q.size()];
    const double m = mean[i % mean.size()];
    const double s = sd[i % sd.size()];
    const double xi_val = xi[i % xi.size()];
    const double y = mlcore::distributions::psnorm(qv, m, s, xi_val);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector qsnorm(const Rcpp::NumericVector &p, const Rcpp::NumericVector &mean,
                 const Rcpp::NumericVector &sd, const Rcpp::NumericVector &xi) {
  const std::size_t n = recycle_length(p.size(), mean.size(),
                                 std::max(sd.size(), xi.size()));
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double pv = p[i % p.size()];
    const double m = mean[i % mean.size()];
    const double s = sd[i % sd.size()];
    const double xi_val = xi[i % xi.size()];
    const double y = mlcore::distributions::qsnorm(pv, m, s, xi_val);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector rsnorm(int n, double mean = 0.0, double sd = 1.0, double xi = 1.5) {
  if (n < 0 || !(sd > 0.0 && xi > 0.0)) {
    Rcpp::stop("Invalid parameters for rsnorm");
  }
  Rcpp::NumericVector out(static_cast<std::size_t>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = mlcore::distributions::rsnorm(rng, mean, sd, xi);
  }
  return out;
}

} // namespace dist_impl
