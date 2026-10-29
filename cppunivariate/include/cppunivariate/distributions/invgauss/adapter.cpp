// Rcpp adapter; copied into the assembled R package.

#include "as_span.h"
#include "cppunivariate/distributions/invgauss/mle.hpp"
#include <Rcpp.h>
#include <limits>
#include <cmath>

// [[Rcpp::export]]
Rcpp::List mlinvgauss_(const Rcpp::NumericVector &x,
    Rcpp::Nullable<double> mean = R_NilValue,
    Rcpp::Nullable<double> shape = R_NilValue) {
  Rcpp::CharacterVector fixed(0);
  const double mean_held = mean.isNull() ? std::numeric_limits<double>::quiet_NaN() : Rcpp::as<double>(mean);
  if (mean.isNotNull()) {
    if (!(std::isfinite(mean_held) && mean_held > 0.0)) Rcpp::stop("Held `mean` must be positive and finite.");
    fixed.push_back("mean");
  }
  const double shape_held = shape.isNull() ? std::numeric_limits<double>::quiet_NaN() : Rcpp::as<double>(shape);
  if (shape.isNotNull()) {
    if (!(std::isfinite(shape_held) && shape_held > 0.0)) Rcpp::stop("Held `shape` must be positive and finite.");
    fixed.push_back("shape");
  }

  auto res = cppunivariate::invgauss_mle(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, mean_held, shape_held);
  if (!(res.lambda > 0.0) || !std::isfinite(res.lambda) || !std::isfinite(res.logLik)) {
    Rcpp::stop("Inverse Gaussian MLE failed: constant data have no attained maximum; other data may exceed double precision.");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mean"] = res.mu, Rcpp::_["shape"] = res.lambda),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["fixed"] = fixed);
}
