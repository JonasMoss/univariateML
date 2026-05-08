// Log-logistic distribution: Rcpp::NumericVector wrappers.
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/llogis.hpp"

namespace dist_impl {

Rcpp::NumericVector dllogis(const Rcpp::NumericVector &x, const Rcpp::NumericVector &shape,
                  const Rcpp::NumericVector &rate, bool log = false) {
  const std::size_t n = recycle_length(x.size(), shape.size(), rate.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::dllogis(
        x[i % x.size()], shape[i % shape.size()], rate[i % rate.size()],
        log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector pllogis(const Rcpp::NumericVector &q, const Rcpp::NumericVector &shape,
                  const Rcpp::NumericVector &rate, bool lower_tail = true,
                  bool log_p = false) {
  const std::size_t n = recycle_length(q.size(), shape.size(), rate.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::pllogis(
        q[i % q.size()], shape[i % shape.size()], rate[i % rate.size()],
        lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector qllogis(const Rcpp::NumericVector &p, const Rcpp::NumericVector &shape,
                  const Rcpp::NumericVector &rate, bool lower_tail = true,
                  bool log_p = false) {
  const std::size_t n = recycle_length(p.size(), shape.size(), rate.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::qllogis(
        p[i % p.size()], shape[i % shape.size()], rate[i % rate.size()],
        lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector rllogis(int n, double shape, double rate) {
  if (n < 0 || !(shape > 0.0 && rate > 0.0)) {
    Rcpp::stop("Invalid parameters for rllogis");
  }
  Rcpp::NumericVector out(static_cast<std::size_t>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = cppunivariate::distributions::rllogis(rng, shape, rate);
  }
  return out;
}

} // namespace dist_impl
