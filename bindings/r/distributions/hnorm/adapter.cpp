#include "as_span.h"
#include "cppunivariate/distributions/hnorm/mle.hpp"
#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlhnorm_(const Rcpp::NumericVector &x, Rcpp::Nullable<double> mu,
    Rcpp::Nullable<double> sigma = R_NilValue) {
  const bool fixed = mu.isNotNull();
  const double held = fixed ? Rcpp::as<double>(mu) : std::numeric_limits<double>::quiet_NaN();
  if (fixed && !std::isfinite(held)) Rcpp::stop("`mu` must be finite.");
  const double sc = sigma.isNotNull() ? Rcpp::as<double>(sigma) : std::numeric_limits<double>::quiet_NaN();
  if (sigma.isNotNull() && (!(sc>0) || !std::isfinite(sc))) Rcpp::stop("`sigma` must be positive finite.");
  const auto res = cppunivariate::hnorm_mle(dist_impl::as_span(x), held, sc);
  if (sigma.isNotNull() && res.status == cppunivariate::HnormStatus::Ok) {
    Rcpp::CharacterVector fixed_names(0);
    if (fixed) fixed_names.push_back("mu");
    fixed_names.push_back("sigma");
    const double endpoint = res.mu;
    return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(
      Rcpp::_["mu"]=endpoint,Rcpp::_["sigma"]=sc),Rcpp::_["logLik"]=res.logLik,Rcpp::_["fixed"]=fixed_names);
  }
  if (fixed && res.status == cppunivariate::HnormStatus::DegenerateData)
    Rcpp::stop("Half-normal likelihood is unbounded as sigma -> 0 when all observations equal held mu.");
  if (fixed && res.status != cppunivariate::HnormStatus::Ok)
    Rcpp::stop("Half-normal observations must be finite and at least the held mu.");
  if (res.status == cppunivariate::HnormStatus::DegenerateData)
    Rcpp::stop("Translated half-normal MLE requires at least two observations with distinct values; degenerate data have no positive-scale MLE.");
  if (res.status != cppunivariate::HnormStatus::Ok)
    Rcpp::stop("Translated half-normal MLE requires finite data with a representable range.");
  if (fixed) return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(Rcpp::_["mu"] = res.mu, Rcpp::_["sigma"] = res.sigma),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["fixed"] = Rcpp::CharacterVector::create("mu"));
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mu"] = res.mu, Rcpp::_["sigma"] = res.sigma),
      Rcpp::_["logLik"] = res.logLik);
}
