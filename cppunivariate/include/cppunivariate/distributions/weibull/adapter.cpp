// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/weibull/mle.hpp"
#include <Rcpp.h>
#include <limits>
#include <algorithm>
#include <cmath>
#include <vector>

// [[Rcpp::export]]
Rcpp::List mlweibull_(const Rcpp::NumericVector &x, Rcpp::Nullable<double> shape0 = R_NilValue,
                         double reltol = 1.220703e-4,
                      int iterlim = 100,
    Rcpp::Nullable<double> shape = R_NilValue,
    Rcpp::Nullable<double> scale = R_NilValue) {
  Rcpp::CharacterVector fixed(0);
  const double shape_held = shape.isNull() ? std::numeric_limits<double>::quiet_NaN() : Rcpp::as<double>(shape);
  if (shape.isNotNull()) {
    if (!(std::isfinite(shape_held) && shape_held > 0.0)) Rcpp::stop("Held `shape` must be positive and finite.");
    fixed.push_back("shape");
  }
  const double scale_held = scale.isNull() ? std::numeric_limits<double>::quiet_NaN() : Rcpp::as<double>(scale);
  if (scale.isNotNull()) {
    if (!(std::isfinite(scale_held) && scale_held > 0.0)) Rcpp::stop("Held `scale` must be positive and finite.");
    fixed.push_back("scale");
  }

  double start = std::numeric_limits<double>::quiet_NaN();
  if (shape0.isNotNull()) {
    start = Rcpp::as<double>(shape0);
    if (!(start > 0.0) || !std::isfinite(start)) {
      Rcpp::stop("`shape0` must be a positive finite number.");
    }
  }
  if (iterlim <= 1 && shape.isNull() && scale.isNull()) {
    Rcpp::warning("iteration limit reached");
  }
  auto res = cppunivariate::weibull_mle(dist_impl::as_span(x), reltol, iterlim, start, shape_held, scale_held);
  if (res.status == cppunivariate::WeibullStatus::Unbounded)
    Rcpp::stop("The weibull maximum likelihood estimator does not exist: the likelihood is unbounded when all transformed observations equal the held scale.");
  if (res.status == cppunivariate::WeibullStatus::IterationFailure)
    Rcpp::stop("The held-parameter weibull score solver failed to converge; increase iterlim.");
  if (!std::isfinite(res.shape) || !std::isfinite(res.scale) || !std::isfinite(res.logLik)) {
    if (shape.isNull() && scale.isNull() && std::all_of(x.begin(), x.end(), [&](double v) { return v == x[0]; }))
      Rcpp::stop("The Weibull maximum likelihood estimator does not exist for constant data: the likelihood is unbounded.");
    Rcpp::stop("Weibull MLE failed");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape"] = res.shape, Rcpp::_["scale"] = res.scale),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations, Rcpp::_["fixed"] = fixed);
}
