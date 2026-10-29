#include "as_span.h"
#include "cppunivariate/distributions/invexp/mle.hpp"
#include <Rcpp.h>
#include <limits>
#include <cmath>
// [[Rcpp::export]]
Rcpp::List mlinvexp_(const Rcpp::NumericVector &x,
    Rcpp::Nullable<double> rate = R_NilValue) {
  Rcpp::CharacterVector fixed(0);
  const double rate_held = rate.isNull() ? std::numeric_limits<double>::quiet_NaN() : Rcpp::as<double>(rate);
  if (rate.isNotNull()) {
    if (!(std::isfinite(rate_held) && rate_held > 0.0)) Rcpp::stop("Held `rate` must be positive and finite.");
    fixed.push_back("rate");
  }

  auto r=cppunivariate::invexp_mle(dist_impl::as_span(x), rate_held);
  if (!(r.rate>0.0) || !std::isfinite(r.rate) || !std::isfinite(r.logLik))
    Rcpp::stop("Inverse exponential MLE exceeds double precision.");
  return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["rate"]=r.rate), Rcpp::_["logLik"]=r.logLik, Rcpp::_["iterations"]=0, Rcpp::_["fixed"] = fixed);
}
