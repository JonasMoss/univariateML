// Beta-prime distribution: Rcpp::NumericVector wrappers.
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/betapr.hpp"

namespace dist_impl {

Rcpp::NumericVector dbetapr(const Rcpp::NumericVector &x, const Rcpp::NumericVector &shape1,
                  const Rcpp::NumericVector &shape2, bool log = false) {
  const std::size_t n = recycle_length(x.size(), shape1.size(), shape2.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::dbetapr(
        x[i % x.size()], shape1[i % shape1.size()],
        shape2[i % shape2.size()], log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector pbetapr(const Rcpp::NumericVector &q, const Rcpp::NumericVector &shape1,
                  const Rcpp::NumericVector &shape2, bool lower_tail = true,
                  bool log_p = false) {
  const std::size_t n = recycle_length(q.size(), shape1.size(), shape2.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::pbetapr(
        q[i % q.size()], shape1[i % shape1.size()],
        shape2[i % shape2.size()], lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector qbetapr(const Rcpp::NumericVector &p, const Rcpp::NumericVector &shape1,
                  const Rcpp::NumericVector &shape2, bool lower_tail = true,
                  bool log_p = false) {
  const std::size_t n = recycle_length(p.size(), shape1.size(), shape2.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::qbetapr(
        p[i % p.size()], shape1[i % shape1.size()],
        shape2[i % shape2.size()], lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector rbetapr(int n, double shape1 = 1.0, double shape2 = 1.0) {
  if (n < 0 || !(shape1 > 0.0 && shape2 > 0.0)) {
    Rcpp::stop("Invalid parameters for rbetapr");
  }
  Rcpp::NumericVector out(static_cast<std::size_t>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = cppunivariate::distributions::rbetapr(rng, shape1, shape2);
  }
  return out;
}

} // namespace dist_impl
