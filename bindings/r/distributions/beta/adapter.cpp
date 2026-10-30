// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/beta/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlbeta_(const Rcpp::NumericVector &x, double reltol = 1.490116e-08,
                   int iterlim = 100,
    Rcpp::Nullable<double> shape1 = R_NilValue,
    Rcpp::Nullable<double> shape2 = R_NilValue) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  auto held = [&](Rcpp::Nullable<double> z, bool positive) {
    if (z.isNull()) return nan;
    const double v = Rcpp::as<double>(z);
    if (!std::isfinite(v) || (positive && !(v > 0)))
      Rcpp::stop("Held parameters must be finite and shapes/scales positive.");
    return v;
  };
  Rcpp::CharacterVector fixed(0);
  if (shape1.isNotNull()) fixed.push_back("shape1");
  if (shape2.isNotNull()) fixed.push_back("shape2");
  auto res = cppunivariate::beta_mle(dist_impl::as_span(x), reltol, iterlim, held(shape1, true), held(shape2, true));
  if (res.status == cppunivariate::BetaStatus::Degenerate) {
    Rcpp::stop("The beta maximum likelihood estimator does not exist for constant data: "
               "the likelihood is unbounded as both shapes grow.");
  }
  if (!std::isfinite(res.alpha) || !std::isfinite(res.beta) || !std::isfinite(res.logLik)) {
    Rcpp::stop("Beta MLE failed: iteration limit or shapes cannot be resolved in double precision; increase iterlim.");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape1"] = res.alpha, Rcpp::_["shape2"] = res.beta),
      Rcpp::_["fixed"] = fixed, Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}
