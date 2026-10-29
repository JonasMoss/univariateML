// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/cauchy/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>
#include <vector>

// [[Rcpp::export]]
Rcpp::List mlcauchy_(const Rcpp::NumericVector &x, double abstol = 1.220703e-4,
                     int iterlim = 100) {
  cppunivariate::CauchyResult res = cppunivariate::fit_cauchy(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, abstol,
      iterlim);
  if (res.status == cppunivariate::CauchyStatus::NoMLE)
    Rcpp::stop("Cauchy MLE does not exist: a sample atom has at least half the mass, except for balanced two-point samples");
  if (!std::isfinite(res.mu) || !std::isfinite(res.scale) || !(res.scale > 0.0))
    Rcpp::stop("Cauchy MLE failed: non-finite or invalid parameters");
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["location"] = res.mu, Rcpp::_["scale"] = res.scale),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}
