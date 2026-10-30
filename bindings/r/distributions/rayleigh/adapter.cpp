// Rcpp adapter; copied into the assembled R package.

#include "as_span.h"
#include "cppunivariate/distributions/rayleigh/mle.hpp"
#include <Rcpp.h>
#include <limits>
#include <cmath>

// [[Rcpp::export]]
Rcpp::List mlrayleigh_(const Rcpp::NumericVector &x,
    Rcpp::Nullable<double> sigma = R_NilValue) {
  Rcpp::CharacterVector fixed(0);
  const double sigma_held = sigma.isNull() ? std::numeric_limits<double>::quiet_NaN() : Rcpp::as<double>(sigma);
  if (sigma.isNotNull()) {
    if (!(std::isfinite(sigma_held) && sigma_held > 0.0)) Rcpp::stop("Held `sigma` must be positive and finite.");
    fixed.push_back("sigma");
  }

  auto res = cppunivariate::rayleigh_mle(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, sigma_held);
  if (res.status == cppunivariate::RayleighStatus::ZeroLikelihood) {
    Rcpp::stop("The Rayleigh likelihood is identically zero when any observation is zero; "
               "no scale has positive likelihood.");
  }
  if (res.status != cppunivariate::RayleighStatus::Ok || !std::isfinite(res.logLik)) Rcpp::stop("Invalid Rayleigh data.");
  return Rcpp::List::create(Rcpp::_["estimates"] = Rcpp::NumericVector::create(
                                Rcpp::_["sigma"] = res.sigma),
                            Rcpp::_["logLik"] = res.logLik, Rcpp::_["fixed"] = fixed);
}
