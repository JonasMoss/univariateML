#include "as_span.h"
#include "cppunivariate/distributions/ztpois/mle.hpp"
#include <Rcpp.h>
// [[Rcpp::export]]
Rcpp::List mlztpois_(const Rcpp::IntegerVector &x) {
  const auto res=cppunivariate::ztpois_mle(dist_impl::as_ispan(x));
  if(res.status==cppunivariate::ZtpoisStatus::InvalidInput)Rcpp::stop("requires nonempty positive integer count data and valid controls");
  if(!std::isfinite(res.logLik))Rcpp::stop("MLE cannot be resolved at floating-point precision.");
  return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["lambda"]=res.lambda),
    Rcpp::_["logLik"]=res.logLik,Rcpp::_["iterations"]=res.iterations,
    Rcpp::_["fixed"]=Rcpp::CharacterVector(0));
}
