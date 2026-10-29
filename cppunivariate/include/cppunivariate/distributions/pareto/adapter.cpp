// Rcpp adapter; copied into the assembled R package.

#include "as_span.h"
#include "cppunivariate/distributions/pareto/mle.hpp"
#include <Rcpp.h>
#include <limits>
#include <cmath>
#include <algorithm>

// [[Rcpp::export]]
Rcpp::List mlpareto_(const Rcpp::NumericVector &x,
    Rcpp::Nullable<double> a = R_NilValue,
    Rcpp::Nullable<double> b = R_NilValue) {
  Rcpp::CharacterVector fixed(0);
  const double a_held = a.isNull() ? std::numeric_limits<double>::quiet_NaN() : Rcpp::as<double>(a);
  if (a.isNotNull()) {
    if (!(std::isfinite(a_held) && a_held > 0.0)) Rcpp::stop("Held `a` must be positive and finite.");
    fixed.push_back("a");
  }
  const double b_held = b.isNull() ? std::numeric_limits<double>::quiet_NaN() : Rcpp::as<double>(b);
  if (b.isNotNull()) {
    if (!(std::isfinite(b_held) && b_held > 0.0)) Rcpp::stop("Held `b` must be positive and finite.");
    fixed.push_back("b");
  }
  if (b.isNotNull() && std::any_of(x.begin(),x.end(),[&](double v){return v<b_held;})) Rcpp::stop("Held b exceeds the sample minimum: zero likelihood.");

  auto res = cppunivariate::pareto_mle(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, a_held, b_held);
  if (!(res.shape>0.0) || !std::isfinite(res.shape) || !std::isfinite(res.logLik))
    Rcpp::stop("The Pareto maximum likelihood estimator does not exist for constant data; other data may exceed double precision.");
  return Rcpp::List::create(Rcpp::_["estimates"] = Rcpp::NumericVector::create(
                                Rcpp::_["a"] = res.shape,
                                Rcpp::_["b"] = res.min),
                            Rcpp::_["logLik"] = res.logLik, Rcpp::_["fixed"] = fixed);
}
