#include "as_span.h"
#include "cppunivariate/distributions/ztpois/mle.hpp"
#include <Rcpp.h>
// [[Rcpp::export]]
Rcpp::List mlztpois_(const Rcpp::IntegerVector &x, Rcpp::Nullable<double> lambda = R_NilValue) {
  if(lambda.isNotNull() && !std::isfinite(Rcpp::as<double>(lambda)))Rcpp::stop("`lambda` must be finite and in its parameter domain.");

  const auto res=cppunivariate::ztpois_mle(dist_impl::as_ispan(x),lambda.isNotNull()?Rcpp::as<double>(lambda):std::numeric_limits<double>::quiet_NaN());
  if(res.status==cppunivariate::ZtpoisStatus::InvalidInput)Rcpp::stop("requires nonempty positive integer count data, a held coefficient in its domain, and compatible observations");
  if(!std::isfinite(res.logLik))Rcpp::stop("MLE cannot be resolved at floating-point precision.");
  Rcpp::CharacterVector fixed(0);if(lambda.isNotNull())fixed.push_back("lambda");
  return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["lambda"]=res.lambda),
    Rcpp::_["logLik"]=res.logLik,Rcpp::_["iterations"]=res.iterations,
    Rcpp::_["fixed"]=fixed);
}
