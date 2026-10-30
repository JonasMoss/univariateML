// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/lgamma/mle.hpp"
#include <Rcpp.h>
#include <limits>
#include <cmath>
#include <algorithm>
#include <vector>

// [[Rcpp::export]]
Rcpp::List mllgamma_(const Rcpp::NumericVector &x, double abstol = 1.220703e-4,
                     int iterlim = 100,
    Rcpp::Nullable<double> shapelog = R_NilValue,
    Rcpp::Nullable<double> ratelog = R_NilValue) {
  Rcpp::CharacterVector fixed(0);
  const double shapelog_held = shapelog.isNull() ? std::numeric_limits<double>::quiet_NaN() : Rcpp::as<double>(shapelog);
  if (shapelog.isNotNull()) {
    if (!(std::isfinite(shapelog_held) && shapelog_held > 0.0)) Rcpp::stop("Held `shapelog` must be positive and finite.");
    fixed.push_back("shapelog");
  }
  const double ratelog_held = ratelog.isNull() ? std::numeric_limits<double>::quiet_NaN() : Rcpp::as<double>(ratelog);
  if (ratelog.isNotNull()) {
    if (!(std::isfinite(ratelog_held) && ratelog_held > 0.0)) Rcpp::stop("Held `ratelog` must be positive and finite.");
    fixed.push_back("ratelog");
  }

  const auto res=cppunivariate::lgamma_mle(dist_impl::as_span(x),abstol,iterlim,shapelog_held,ratelog_held);
  if(res.status==cppunivariate::LgammaStatus::IterationFailure)
    Rcpp::stop("Gamma delegate optimizer IterationFailure; increase iterlim.");
  if(res.status==cppunivariate::LgammaStatus::TransformFailure)
    Rcpp::stop("lgamma MLE failed: transformed observations exceed double precision.");
  if(res.status==cppunivariate::LgammaStatus::Degenerate)
    Rcpp::stop("The loggamma maximum likelihood estimator does not exist for constant data: the likelihood is unbounded as the shape grows.");
  if(!std::isfinite(res.shapelog) || !std::isfinite(res.ratelog) || !std::isfinite(res.logLik))
    Rcpp::stop("Loggamma MLE failed");
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shapelog"] = res.shapelog, Rcpp::_["ratelog"] = res.ratelog),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations, Rcpp::_["fixed"] = fixed);
}
