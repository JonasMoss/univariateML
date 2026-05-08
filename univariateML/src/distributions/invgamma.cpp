// Inverse-Gamma distribution: Rcpp::NumericVector wrappers.
#include "common.h"
#include "rng_r.h"
#include "cppunivariate/distributions/invgamma/dpqr.hpp"

namespace dist_impl {

Rcpp::NumericVector dinvgamma(const Rcpp::NumericVector &x, const Rcpp::NumericVector &alpha,
                    const Rcpp::NumericVector &beta, bool log = false) {
  const std::size_t n = recycle_length(x.size(), alpha.size(), beta.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::dinvgamma(
        x[i % x.size()], alpha[i % alpha.size()], beta[i % beta.size()], log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector pinvgamma(const Rcpp::NumericVector &q, const Rcpp::NumericVector &alpha,
                    const Rcpp::NumericVector &beta, bool lower_tail = true,
                    bool log_p = false) {
  const std::size_t n = recycle_length(q.size(), alpha.size(), beta.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::pinvgamma(
        q[i % q.size()], alpha[i % alpha.size()], beta[i % beta.size()],
        lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector qinvgamma(const Rcpp::NumericVector &p, const Rcpp::NumericVector &alpha,
                    const Rcpp::NumericVector &beta, bool lower_tail = true,
                    bool log_p = false) {
  const std::size_t n = recycle_length(p.size(), alpha.size(), beta.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::qinvgamma(
        p[i % p.size()], alpha[i % alpha.size()], beta[i % beta.size()],
        lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector rinvgamma(int n, double alpha = 1.0, double beta = 1.0) {
  if (n < 0 || !(alpha > 0.0 && beta > 0.0)) {
    Rcpp::stop("Invalid parameters for rinvgamma");
  }
  Rcpp::NumericVector out(static_cast<std::size_t>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = cppunivariate::distributions::rinvgamma(rng, alpha, beta);
  }
  return out;
}

} // namespace dist_impl
