// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/gamma/mle.hpp"
#include <Rcpp.h>
#include <limits>
#include <cmath>
#include <vector>

// [[Rcpp::export]]
Rcpp::List mlgamma_(const Rcpp::NumericVector &x, double abstol = 1.220703e-4,
                    int iterlim = 100,
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

  auto res = cppunivariate::gamma_mle(dist_impl::as_span(x), abstol, iterlim, shape_held, rate_held);
  if (res.status == cppunivariate::GammaStatus::IterationFailure)
    Rcpp::stop("Gamma delegate optimizer IterationFailure; increase iterlim.");
  if (res.status == cppunivariate::GammaStatus::Degenerate) {
    Rcpp::stop("The gamma maximum likelihood estimator does not exist for constant data: "
               "the likelihood is unbounded as the shape grows.");
  }
  if (!std::isfinite(res.shape) || !std::isfinite(res.rate) || !std::isfinite(res.logLik)) {
    Rcpp::stop("Gamma MLE failed");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape"] = res.shape, Rcpp::_["rate"] = res.rate),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations, Rcpp::_["fixed"] = fixed);
}
