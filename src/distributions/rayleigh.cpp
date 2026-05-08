// Rayleigh distribution: Rcpp::NumericVector wrappers around cppunivariate::distributions.
#include "common.h"
#include "rng_r.h"
#include "../cppunivariate/distributions/rayleigh.hpp"

namespace dist_impl {

Rcpp::NumericVector drayleigh(const Rcpp::NumericVector &x, const Rcpp::NumericVector &sigma,
                    bool log = false) {
  const std::size_t n = recycle_length(x.size(), sigma.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::drayleigh(
        x[i % x.size()], sigma[i % sigma.size()], log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector prayleigh(const Rcpp::NumericVector &q, const Rcpp::NumericVector &sigma,
                    bool lower_tail = true, bool log_p = false) {
  const std::size_t n = recycle_length(q.size(), sigma.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::prayleigh(
        q[i % q.size()], sigma[i % sigma.size()], lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector qrayleigh(const Rcpp::NumericVector &p, const Rcpp::NumericVector &sigma,
                    bool lower_tail = true, bool log_p = false) {
  const std::size_t n = recycle_length(p.size(), sigma.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::qrayleigh(
        p[i % p.size()], sigma[i % sigma.size()], lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector rrayleigh(int n, double sigma = 1.0) {
  if (n < 0 || !(sigma > 0.0)) {
    Rcpp::stop("Invalid parameters for rrayleigh");
  }
  Rcpp::NumericVector out(static_cast<std::size_t>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = cppunivariate::distributions::rrayleigh(rng, sigma);
  }
  return out;
}

} // namespace dist_impl
