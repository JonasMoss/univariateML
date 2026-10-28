// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/betapr/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlbetapr_(const Rcpp::NumericVector &x, double reltol = 1.490116e-08,
                     int iterlim = 100) {
  cppunivariate::BetaPrimeResult res =
      cppunivariate::betapr_mle(dist_impl::as_span(x), reltol, iterlim);
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape1"] = res.alpha, Rcpp::_["shape2"] = res.beta),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}
