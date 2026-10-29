#include "as_span.h"
#include "cppunivariate/distributions/ztgeom/mle.hpp"
#include <Rcpp.h>
// [[Rcpp::export]]
Rcpp::List mlztgeom_(const Rcpp::IntegerVector &x) {
  const auto res=cppunivariate::ztgeom_mle(dist_impl::as_ispan(x));
  if(res.status==cppunivariate::ZtgeomStatus::InvalidInput)Rcpp::stop("requires nonempty positive integer count data and valid controls");
  if(!std::isfinite(res.logLik))Rcpp::stop("MLE cannot be resolved at floating-point precision.");
  return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["prob"]=res.prob),
    Rcpp::_["logLik"]=res.logLik,Rcpp::_["iterations"]=res.iterations,
    Rcpp::_["fixed"]=Rcpp::CharacterVector(0));
}
