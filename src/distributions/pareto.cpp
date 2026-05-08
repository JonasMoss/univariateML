// Pareto distribution: Rcpp::NumericVector wrappers.
#include "common.h"
#include "rng_r.h"
#include "../cppunivariate/distributions/pareto.hpp"

namespace dist_impl {

Rcpp::NumericVector dpareto(const Rcpp::NumericVector &x, const Rcpp::NumericVector &a, const Rcpp::NumericVector &b,
                  bool log = false) {
  const std::size_t n = recycle_length(x.size(), a.size(), b.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::dpareto(
        x[i % x.size()], a[i % a.size()], b[i % b.size()], log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector ppareto(const Rcpp::NumericVector &q, const Rcpp::NumericVector &a, const Rcpp::NumericVector &b,
                  bool lower_tail = true, bool log_p = false) {
  const std::size_t n = recycle_length(q.size(), a.size(), b.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::ppareto(
        q[i % q.size()], a[i % a.size()], b[i % b.size()], lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector qpareto(const Rcpp::NumericVector &p, const Rcpp::NumericVector &a, const Rcpp::NumericVector &b,
                  bool lower_tail = true, bool log_p = false) {
  const std::size_t n = recycle_length(p.size(), a.size(), b.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::qpareto(
        p[i % p.size()], a[i % a.size()], b[i % b.size()], lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector rpareto(int n, double a = 1.0, double b = 1.0) {
  if (n < 0 || !(a > 0.0 && b > 0.0)) {
    Rcpp::stop("Invalid parameters for rpareto");
  }
  Rcpp::NumericVector out(static_cast<std::size_t>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = cppunivariate::distributions::rpareto(rng, a, b);
  }
  return out;
}

} // namespace dist_impl
