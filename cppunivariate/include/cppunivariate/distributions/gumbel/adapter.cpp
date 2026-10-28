// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/gumbel/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>
#include <vector>

// [[Rcpp::export]]
Rcpp::List mlgumbel_(const Rcpp::NumericVector &x, double reltol = 1.220703e-4,
                     int iterlim = 100) {
  if (iterlim <= 1) {
    Rcpp::warning("iteration limit reached");
  }
  cppunivariate::GumbelResult res = cppunivariate::fit_gumbel_newton(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, reltol,
      iterlim);
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mu"] = res.mu, Rcpp::_["sigma"] = res.sigma),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}
