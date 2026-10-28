#include "as_span.h"
#include "cppunivariate/distributions/invexp/mle.hpp"
#include <Rcpp.h>
// [[Rcpp::export]]
Rcpp::List mlinvexp_(const Rcpp::NumericVector &x) {
  auto r=cppunivariate::invexp_mle(dist_impl::as_span(x));
  return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["rate"]=r.rate), Rcpp::_["logLik"]=r.logLik, Rcpp::_["iterations"]=0);
}
