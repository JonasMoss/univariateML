// Birnbaum-Saunders (fatigue) distribution: Rcpp::NumericVector wrappers.
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/fatigue.hpp"

namespace dist_impl {

Rcpp::NumericVector dfatigue(const Rcpp::NumericVector &x, const Rcpp::NumericVector &alpha,
                   const Rcpp::NumericVector &beta, const Rcpp::NumericVector &mu,
                   bool log = false) {
  const std::size_t n = recycle_length(x.size(), alpha.size(),
                                 std::max(beta.size(), mu.size()));
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::dfatigue(
        x[i % x.size()], alpha[i % alpha.size()], beta[i % beta.size()],
        mu[i % mu.size()], log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector pfatigue(const Rcpp::NumericVector &q, const Rcpp::NumericVector &alpha,
                   const Rcpp::NumericVector &beta, const Rcpp::NumericVector &mu,
                   bool lower_tail = true, bool log_p = false) {
  const std::size_t n = recycle_length(q.size(), alpha.size(),
                                 std::max(beta.size(), mu.size()));
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::pfatigue(
        q[i % q.size()], alpha[i % alpha.size()], beta[i % beta.size()],
        mu[i % mu.size()], lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector qfatigue(const Rcpp::NumericVector &p, const Rcpp::NumericVector &alpha,
                   const Rcpp::NumericVector &beta, const Rcpp::NumericVector &mu,
                   bool lower_tail = true, bool log_p = false) {
  const std::size_t n = recycle_length(p.size(), alpha.size(),
                                 std::max(beta.size(), mu.size()));
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::qfatigue(
        p[i % p.size()], alpha[i % alpha.size()], beta[i % beta.size()],
        mu[i % mu.size()], lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector rfatigue(int n, double alpha = 1.0, double beta = 1.0,
                   double mu = 0.0) {
  if (n < 0 || !(alpha > 0.0 && beta > 0.0)) {
    Rcpp::stop("Invalid parameters for rfatigue");
  }
  Rcpp::NumericVector out(static_cast<std::size_t>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = cppunivariate::distributions::rfatigue(rng, alpha, beta, mu);
  }
  return out;
}

} // namespace dist_impl
