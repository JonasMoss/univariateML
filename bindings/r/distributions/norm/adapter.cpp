// Rcpp adapter; copied into the assembled R package.

#include "as_span.h"
#include "cppunivariate/distributions/norm/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlnorm_(const Rcpp::NumericVector &x, Rcpp::Nullable<double> mean = R_NilValue, Rcpp::Nullable<double> sd = R_NilValue) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double held_mean=mean.isNotNull()?Rcpp::as<double>(mean):nan;
  if(mean.isNotNull() && (!std::isfinite(held_mean)))Rcpp::stop("`mean` must be finite.");
  const double held_sd=sd.isNotNull()?Rcpp::as<double>(sd):nan;
  if(sd.isNotNull() && (!std::isfinite(held_sd) || !(held_sd>0)))Rcpp::stop("`sd` must be positive and finite.");
  Rcpp::CharacterVector fixed;
  if(mean.isNotNull())fixed.push_back("mean");
  if(sd.isNotNull())fixed.push_back("sd");

  auto res =
      cppunivariate::norm_mle(dist_impl::as_span(x),held_mean,held_sd);
  if (!(res.sd > 0.0) || !std::isfinite(res.sd)) {
    Rcpp::stop("The norm maximum likelihood estimator does not exist for constant data: the likelihood is unbounded as scale shrinks to zero.");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mean"] = res.mean, Rcpp::_["sd"] = res.sd),
      Rcpp::_["fixed"] = fixed, Rcpp::_["logLik"] = res.logLik);
}
