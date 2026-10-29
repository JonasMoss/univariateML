#include "as_span.h"
#include "cppunivariate/distributions/zmpois/mle.hpp"
#include <Rcpp.h>
// [[Rcpp::export]]
Rcpp::List mlzmpois_(const Rcpp::IntegerVector &x) {
  const auto res=cppunivariate::zmpois_mle(dist_impl::as_ispan(x));
  if(res.status==cppunivariate::ZmpoisStatus::InvalidInput)Rcpp::stop("requires nonempty nonnegative integer count data and valid controls");
  if(res.status==cppunivariate::ZmpoisStatus::AllZero)Rcpp::warning("all observations are zero: base parameters are unidentified; returning p0 = 1.");
  if(res.status==cppunivariate::ZmpoisStatus::TwoPoint)Rcpp::warning("positives are all one: the attained two-point law is also fitted exactly by mlbinom(x, size = 1).");
  if(!std::isfinite(res.logLik))Rcpp::stop("MLE cannot be resolved at floating-point precision.");
  return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["lambda"]=res.lambda,Rcpp::_["p0"]=res.p0),
    Rcpp::_["logLik"]=res.logLik,Rcpp::_["iterations"]=res.iterations,
    Rcpp::_["fixed"]=Rcpp::CharacterVector(0));
}
