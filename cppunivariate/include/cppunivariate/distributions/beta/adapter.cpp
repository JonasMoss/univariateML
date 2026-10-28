// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/beta/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlbeta_(const Rcpp::NumericVector &x, double reltol = 1.490116e-08,
                   int iterlim = 100) {
  auto res = cppunivariate::beta_mle(dist_impl::as_span(x), reltol, iterlim);
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape1"] = res.alpha, Rcpp::_["shape2"] = res.beta),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}
