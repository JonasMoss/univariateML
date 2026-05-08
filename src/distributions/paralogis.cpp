// Paralogistic distribution: Rcpp::NumericVector wrappers.
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/paralogis.hpp"

namespace dist_impl {

Rcpp::NumericVector dparalogis(const Rcpp::NumericVector &x, const Rcpp::NumericVector &shape,
                     const Rcpp::NumericVector &scale, bool log = false) {
  const std::size_t n = recycle_length(x.size(), shape.size(), scale.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::dparalogis(
        x[i % x.size()], shape[i % shape.size()],
        scale[i % scale.size()], log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector pparalogis(const Rcpp::NumericVector &q, const Rcpp::NumericVector &shape,
                     const Rcpp::NumericVector &scale, bool lower_tail = true,
                     bool log_p = false) {
  const std::size_t n = recycle_length(q.size(), shape.size(), scale.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::pparalogis(
        q[i % q.size()], shape[i % shape.size()],
        scale[i % scale.size()], lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector qparalogis(const Rcpp::NumericVector &p, const Rcpp::NumericVector &shape,
                     const Rcpp::NumericVector &scale, bool lower_tail = true,
                     bool log_p = false) {
  const std::size_t n = recycle_length(p.size(), shape.size(), scale.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double y = cppunivariate::distributions::qparalogis(
        p[i % p.size()], shape[i % shape.size()],
        scale[i % scale.size()], lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector rparalogis(int n, double shape = 1.0, double scale = 1.0) {
  if (n < 0 || !(shape > 0.0 && scale > 0.0)) {
    Rcpp::stop("Invalid parameters for rparalogis");
  }
  Rcpp::NumericVector out(static_cast<std::size_t>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = cppunivariate::distributions::rparalogis(rng, shape, scale);
  }
  return out;
}

} // namespace dist_impl
