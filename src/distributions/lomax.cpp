// Lomax distribution: Rcpp::NumericVector wrappers.
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/lomax.hpp"

namespace dist_impl {

Rcpp::NumericVector dlomax(const Rcpp::NumericVector &x, const Rcpp::NumericVector &lambda,
                 const Rcpp::NumericVector &kappa, bool log = false) {
  const std::size_t n = recycle_length(x.size(), lambda.size(), kappa.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = mlcore::distributions::dlomax(
        x[i % x.size()], lambda[i % lambda.size()], kappa[i % kappa.size()],
        log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector plomax(const Rcpp::NumericVector &q, const Rcpp::NumericVector &lambda,
                 const Rcpp::NumericVector &kappa, bool lower_tail = true,
                 bool log_p = false) {
  const std::size_t n = recycle_length(q.size(), lambda.size(), kappa.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = mlcore::distributions::plomax(
        q[i % q.size()], lambda[i % lambda.size()], kappa[i % kappa.size()],
        lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector qlomax(const Rcpp::NumericVector &p, const Rcpp::NumericVector &lambda,
                 const Rcpp::NumericVector &kappa, bool lower_tail = true,
                 bool log_p = false) {
  const std::size_t n = recycle_length(p.size(), lambda.size(), kappa.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = mlcore::distributions::qlomax(
        p[i % p.size()], lambda[i % lambda.size()], kappa[i % kappa.size()],
        lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector rlomax(int n, double lambda = 1.0, double kappa = 1.0) {
  if (n < 0 || !(lambda > 0.0 && kappa > 0.0)) {
    Rcpp::stop("Invalid parameters for rlomax");
  }
  Rcpp::NumericVector out(static_cast<std::size_t>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = mlcore::distributions::rlomax(rng, lambda, kappa);
  }
  return out;
}

} // namespace dist_impl
