// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/gompertz/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlgompertz_(const Rcpp::NumericVector &x, double reltol = 1.220703e-4,
                       int iterlim = 100) {
  auto res = cppunivariate::gompertz_mle(dist_impl::as_span(x), reltol, iterlim);
  if (res.status == cppunivariate::GompertzStatus::Overdispersed) {
    Rcpp::stop("The maximum likelihood estimator of the b parameter Gompertz "
               "distribution does not exist since the data is overdispersed. "
               "The exponential distribution will yield a better fit.");
  }
  if (res.status != cppunivariate::GompertzStatus::Ok || !std::isfinite(res.a) ||
      !std::isfinite(res.b)) {
    Rcpp::stop("Gompertz MLE failed");
  }
  return Rcpp::List::create(Rcpp::_["estimates"] = Rcpp::NumericVector::create(
                                Rcpp::_["a"] = res.a, Rcpp::_["b"] = res.b),
                            Rcpp::_["logLik"] = res.logLik,
                            Rcpp::_["iterations"] = res.iterations);
}
