// Rcpp adapter; copied into the assembled R package.

#include "as_span.h"
#include "cppunivariate/distributions/exp/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <algorithm>

// [[Rcpp::export]]
Rcpp::List mlexp_(const Rcpp::NumericVector &x) {
  auto res = cppunivariate::exp_mle(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())});
  if (!std::isfinite(res.rate) && std::any_of(x.begin(), x.end(), [](double v) { return v > 0.0; })) {
    Rcpp::stop("Exponential MLE rate exceeds double precision.");
  }
  if (!std::isfinite(res.rate)) {
    Rcpp::stop("The exponential maximum likelihood estimator does not exist when every "
               "observation is zero: the likelihood is unbounded as the rate grows.");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] =
          Rcpp::NumericVector::create(Rcpp::_["rate"] = res.rate),
      Rcpp::_["logLik"] = res.logLik);
}
