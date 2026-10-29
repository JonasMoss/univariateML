// Rcpp adapter; copied into the assembled R package.

#include "as_span.h"
#include "cppunivariate/distributions/unif/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlunif_(const Rcpp::NumericVector &x,
    Rcpp::Nullable<double> min = R_NilValue,
    Rcpp::Nullable<double> max = R_NilValue) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  auto held = [&](Rcpp::Nullable<double> z, bool positive) {
    if (z.isNull()) return nan;
    const double v = Rcpp::as<double>(z);
    if (!std::isfinite(v) || (positive && !(v > 0)))
      Rcpp::stop("Held parameters must be finite and shapes/scales positive.");
    return v;
  };
  Rcpp::CharacterVector fixed(0);
  if (min.isNotNull()) fixed.push_back("min");
  if (max.isNotNull()) fixed.push_back("max");
  auto res = cppunivariate::unif_mle(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, held(min, false), held(max, false));
  if (res.status == cppunivariate::UnifStatus::InvalidInput)
    Rcpp::stop("Invalid held uniform endpoints: require finite min < max.");
  if (res.status == cppunivariate::UnifStatus::Incompatible)
    Rcpp::stop("Data outside held uniform endpoints.");
  if (!std::isfinite(res.min) || !std::isfinite(res.max)) {
    Rcpp::stop("The uniform maximum likelihood estimator does not exist for constant data: "
               "the likelihood is unbounded as max - min shrinks to zero.");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["min"] = res.min, Rcpp::_["max"] = res.max),
      Rcpp::_["fixed"] = fixed, Rcpp::_["logLik"] = res.logLik);
}
