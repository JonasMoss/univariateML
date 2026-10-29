// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/naka/mle.hpp"
#include <Rcpp.h>
#include <limits>
#include <cmath>
#include <algorithm>
#include <vector>

// [[Rcpp::export]]
Rcpp::List mlnaka_(const Rcpp::NumericVector &x, double abstol = 1.220703e-4,
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

  auto res = cppunivariate::naka_mle(dist_impl::as_span(x), abstol, iterlim, shape_held, scale_held);
  if (res.status == cppunivariate::GammaStatus::IterationFailure)
    Rcpp::stop("Gamma delegate optimizer IterationFailure; increase iterlim.");
  if (res.status == cppunivariate::GammaStatus::Degenerate) {
    if (!std::all_of(x.begin(), x.end(), [&](double v) { return v == x[0]; }))
      Rcpp::stop("naka MLE failed: transformed observations exceed double precision.");
    Rcpp::stop("The Nakagami maximum likelihood estimator does not exist for constant data: "
               "the likelihood is unbounded as the shape grows.");
  }
  if (!std::isfinite(res.shape) || !std::isfinite(res.scale) || !std::isfinite(res.logLik)) {
    Rcpp::stop("Nakagami MLE failed");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape"] = res.shape, Rcpp::_["scale"] = res.scale),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations, Rcpp::_["fixed"] = fixed);
}
