// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/invweibull/mle.hpp"
#include <Rcpp.h>
#include <limits>
#include <algorithm>
#include <cmath>
#include <vector>

// [[Rcpp::export]]
Rcpp::List mlinvweibull_(const Rcpp::NumericVector &x,
                         Rcpp::Nullable<double> shape0 = R_NilValue,
                         double reltol = 1.220703e-4, int iterlim = 100,
    Rcpp::Nullable<double> shape = R_NilValue,
    Rcpp::Nullable<double> rate = R_NilValue) {
  Rcpp::CharacterVector fixed(0);
  const double shape_held = shape.isNull() ? std::numeric_limits<double>::quiet_NaN() : Rcpp::as<double>(shape);
  if (shape.isNotNull()) {
    if (!(std::isfinite(shape_held) && shape_held > 0.0)) Rcpp::stop("Held `shape` must be positive and finite.");
    fixed.push_back("shape");
  }
  const double rate_held = rate.isNull() ? std::numeric_limits<double>::quiet_NaN() : Rcpp::as<double>(rate);
  if (rate.isNotNull()) {
    if (!(std::isfinite(rate_held) && rate_held > 0.0)) Rcpp::stop("Held `rate` must be positive and finite.");
    fixed.push_back("rate");
  }

  double start = std::numeric_limits<double>::quiet_NaN();
  if (shape0.isNotNull()) {
    start = Rcpp::as<double>(shape0);
    if (!(start > 0.0) || !std::isfinite(start)) {
      Rcpp::stop("`shape0` must be a positive finite number.");
    }
  }
  if (iterlim <= 1 && shape.isNull() && rate.isNull()) {
    Rcpp::warning("iteration limit reached");
  }
  auto res = cppunivariate::invweibull_mle(dist_impl::as_span(x), reltol, iterlim, start, shape_held, rate_held);
  if (res.status == cppunivariate::InvWeibullStatus::Unbounded)
    Rcpp::stop("The invweibull maximum likelihood estimator does not exist: the likelihood is unbounded when all transformed observations equal the held scale.");
  if (res.status == cppunivariate::InvWeibullStatus::IterationFailure)
    Rcpp::stop("The held-parameter invweibull score solver failed to converge; increase iterlim.");
  if (!std::isfinite(res.shape) || !std::isfinite(res.rate) || !std::isfinite(res.logLik)) {
    if (shape.isNull() && rate.isNull() && std::all_of(x.begin(), x.end(), [&](double v) { return v == x[0]; }))
      Rcpp::stop("The inverse Weibull maximum likelihood estimator does not exist for constant data: the likelihood is unbounded.");
    Rcpp::stop("Inverse Weibull MLE failed");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape"] = res.shape, Rcpp::_["rate"] = res.rate),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations, Rcpp::_["fixed"] = fixed);
}
