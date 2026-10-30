// Rcpp adapter; copied into the assembled R package.

#include "as_span.h"
#include "cppunivariate/distributions/exp/mle.hpp"
#include <Rcpp.h>
#include <limits>
#include <cmath>
#include <algorithm>

// [[Rcpp::export]]
Rcpp::List mlexp_(const Rcpp::NumericVector &x,
    Rcpp::Nullable<double> rate = R_NilValue) {
  Rcpp::CharacterVector fixed(0);
  const double rate_held = rate.isNull() ? std::numeric_limits<double>::quiet_NaN() : Rcpp::as<double>(rate);
  if (rate.isNotNull()) {
    if (!(std::isfinite(rate_held) && rate_held > 0.0)) Rcpp::stop("Held `rate` must be positive and finite.");
    fixed.push_back("rate");
  }

  auto res = cppunivariate::exp_mle(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, rate_held);
  if (!std::isfinite(res.rate) && std::any_of(x.begin(), x.end(), [](double v) { return v > 0.0; })) {
    Rcpp::stop("Exponential MLE rate exceeds double precision.");
  }
  if (!std::isfinite(res.rate)) {
    Rcpp::stop("The exponential maximum likelihood estimator does not exist when every "
               "observation is zero: the likelihood is unbounded as the rate grows.");
  }
  if (!std::isfinite(res.logLik)) Rcpp::stop("Exponential likelihood exceeds double precision.");
  return Rcpp::List::create(
      Rcpp::_["estimates"] =
          Rcpp::NumericVector::create(Rcpp::_["rate"] = res.rate),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["fixed"] = fixed);
}
