// Rcpp adapter; copied into the assembled R package.

#include "as_span.h"
#include "cppunivariate/distributions/rayleigh/mle.hpp"
#include <Rcpp.h>
#include <cmath>

// [[Rcpp::export]]
Rcpp::List mlrayleigh_(const Rcpp::NumericVector &x) {
  auto res = cppunivariate::rayleigh_mle(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())});
  if (res.status == cppunivariate::RayleighStatus::ZeroLikelihood) {
    Rcpp::stop("The Rayleigh likelihood is identically zero when any observation is zero; "
               "no scale has positive likelihood.");
  }
  if (res.status != cppunivariate::RayleighStatus::Ok) Rcpp::stop("Invalid Rayleigh data.");
  return Rcpp::List::create(Rcpp::_["estimates"] = Rcpp::NumericVector::create(
                                Rcpp::_["sigma"] = res.sigma),
                            Rcpp::_["logLik"] = res.logLik);
}
