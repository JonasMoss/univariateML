// Loggamma distribution (the *distribution*, not the function): Rcpp::NumericVector
// wrappers. Exposed as dlgamma/plgamma/qlgamma/rlgamma to match actuar.
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/lgamma_dist.hpp"

namespace dist_impl {

Rcpp::NumericVector dlgamma(const Rcpp::NumericVector &x, const Rcpp::NumericVector &shapelog,
                  const Rcpp::NumericVector &ratelog, bool log = false) {
  const std::size_t n = recycle_length(x.size(), shapelog.size(), ratelog.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::dlgamma(
        x[i % x.size()], shapelog[i % shapelog.size()],
        ratelog[i % ratelog.size()], log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector plgamma(const Rcpp::NumericVector &q, const Rcpp::NumericVector &shapelog,
                  const Rcpp::NumericVector &ratelog, bool lower_tail = true,
                  bool log_p = false) {
  const std::size_t n = recycle_length(q.size(), shapelog.size(), ratelog.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::plgamma(
        q[i % q.size()], shapelog[i % shapelog.size()],
        ratelog[i % ratelog.size()], lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector qlgamma(const Rcpp::NumericVector &p, const Rcpp::NumericVector &shapelog,
                  const Rcpp::NumericVector &ratelog, bool lower_tail = true,
                  bool log_p = false) {
  const std::size_t n = recycle_length(p.size(), shapelog.size(), ratelog.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::qlgamma(
        p[i % p.size()], shapelog[i % shapelog.size()],
        ratelog[i % ratelog.size()], lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector rlgamma(int n, double shapelog, double ratelog) {
  if (n < 0 || !(shapelog > 0.0 && ratelog > 0.0)) {
    Rcpp::stop("Invalid parameters for rlgamma");
  }
  Rcpp::NumericVector out(static_cast<std::size_t>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = cppunivariate::distributions::rlgamma(rng, shapelog, ratelog);
  }
  return out;
}

} // namespace dist_impl
