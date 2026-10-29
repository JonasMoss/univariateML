// Rcpp adapter; copied into the assembled R package.

#include "as_span.h"
#include "cppunivariate/distributions/pareto/mle.hpp"
#include <Rcpp.h>
#include <cmath>

// [[Rcpp::export]]
Rcpp::List mlpareto_(const Rcpp::NumericVector &x) {
  auto res = cppunivariate::pareto_mle(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())});
  if (!(res.shape>0.0) || !std::isfinite(res.shape) || !std::isfinite(res.logLik))
    Rcpp::stop("The Pareto maximum likelihood estimator does not exist for constant data; other data may exceed double precision.");
  return Rcpp::List::create(Rcpp::_["estimates"] = Rcpp::NumericVector::create(
                                Rcpp::_["a"] = res.shape,
                                Rcpp::_["b"] = res.min),
                            Rcpp::_["logLik"] = res.logLik);
}
