// Laplace distribution: Rcpp::NumericVector wrappers around cppunivariate::distributions.
#include "common.h"
#include "rng_r.h"
#include "cppunivariate/distributions/laplace.hpp"

namespace dist_impl {

Rcpp::NumericVector dlaplace(const Rcpp::NumericVector &x, const Rcpp::NumericVector &mu,
                   const Rcpp::NumericVector &sigma, bool log = false) {
  const std::size_t n = recycle_length(x.size(), mu.size(), sigma.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::dlaplace(
        x[i % x.size()], mu[i % mu.size()], sigma[i % sigma.size()], log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector plaplace(const Rcpp::NumericVector &q, const Rcpp::NumericVector &mu,
                   const Rcpp::NumericVector &sigma, bool lower_tail = true,
                   bool log_p = false) {
  const std::size_t n = recycle_length(q.size(), mu.size(), sigma.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::plaplace(
        q[i % q.size()], mu[i % mu.size()], sigma[i % sigma.size()],
        lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector qlaplace(const Rcpp::NumericVector &p, const Rcpp::NumericVector &mu,
                   const Rcpp::NumericVector &sigma, bool lower_tail = true,
                   bool log_p = false) {
  const std::size_t n = recycle_length(p.size(), mu.size(), sigma.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::qlaplace(
        p[i % p.size()], mu[i % mu.size()], sigma[i % sigma.size()],
        lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector rlaplace(int n, double mu = 0.0, double sigma = 1.0) {
  if (n < 0 || !(sigma > 0.0)) {
    Rcpp::stop("Invalid parameters for rlaplace");
  }
  Rcpp::NumericVector out(static_cast<std::size_t>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = cppunivariate::distributions::rlaplace(rng, mu, sigma);
  }
  return out;
}

} // namespace dist_impl
