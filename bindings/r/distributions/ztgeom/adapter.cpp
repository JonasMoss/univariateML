#include "as_span.h"
#include "cppunivariate/distributions/ztgeom/mle.hpp"
#include <Rcpp.h>
// [[Rcpp::export]]
Rcpp::List mlztgeom_(const Rcpp::IntegerVector &x, Rcpp::Nullable<double> prob = R_NilValue) {
  if(prob.isNotNull() && !std::isfinite(Rcpp::as<double>(prob)))Rcpp::stop("`prob` must be finite and in its parameter domain.");

  const auto res=cppunivariate::ztgeom_mle(dist_impl::as_ispan(x),prob.isNotNull()?Rcpp::as<double>(prob):std::numeric_limits<double>::quiet_NaN());
  if(res.status==cppunivariate::ZtgeomStatus::InvalidInput)Rcpp::stop("requires nonempty positive integer count data, a held coefficient in its domain, and compatible observations");
  if(!std::isfinite(res.logLik))Rcpp::stop("MLE cannot be resolved at floating-point precision.");
  Rcpp::CharacterVector fixed(0);if(prob.isNotNull())fixed.push_back("prob");
  return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["prob"]=res.prob),
    Rcpp::_["logLik"]=res.logLik,Rcpp::_["iterations"]=res.iterations,
    Rcpp::_["fixed"]=fixed);
}
