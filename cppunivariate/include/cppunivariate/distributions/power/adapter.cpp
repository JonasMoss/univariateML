// Rcpp adapter; copied into the assembled R package.

#include "as_span.h"
#include "cppunivariate/distributions/power/mle.hpp"
#include <Rcpp.h>
#include <cmath>

// [[Rcpp::export]]
Rcpp::List mlpower_(const Rcpp::NumericVector &x,
                    double epsilon = 1.490116e-08) {
  if (!(epsilon>=0.0) || !std::isfinite(epsilon))
    Rcpp::stop("`epsilon` must be nonnegative and finite.");
  auto res = cppunivariate::power_mle(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, epsilon);
  if (!std::isfinite(res.alpha) || !std::isfinite(res.beta)) {
    Rcpp::stop("The power distribution is only defined for positive numbers.");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["alpha"] = res.alpha, Rcpp::_["beta"] = res.beta),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}
