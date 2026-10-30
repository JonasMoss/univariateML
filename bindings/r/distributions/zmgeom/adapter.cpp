#include "as_span.h"
#include "cppunivariate/distributions/zmgeom/mle.hpp"
#include <Rcpp.h>
// [[Rcpp::export]]
Rcpp::List mlzmgeom_(const Rcpp::IntegerVector &x, Rcpp::Nullable<double> prob = R_NilValue, Rcpp::Nullable<double> p0 = R_NilValue) {
  if(prob.isNotNull() && !std::isfinite(Rcpp::as<double>(prob)))Rcpp::stop("`prob` must be finite and in its parameter domain.");
  if(p0.isNotNull() && !std::isfinite(Rcpp::as<double>(p0)))Rcpp::stop("`p0` must be finite and in its parameter domain.");

  const auto res=cppunivariate::zmgeom_mle(dist_impl::as_ispan(x),1e-10,200,prob.isNotNull()?Rcpp::as<double>(prob):std::numeric_limits<double>::quiet_NaN(),p0.isNotNull()?Rcpp::as<double>(p0):std::numeric_limits<double>::quiet_NaN());
  if(res.status==cppunivariate::ZmgeomStatus::InvalidInput)Rcpp::stop("requires nonempty nonnegative integer count data and valid controls and parameter domains; held values must be compatible with the observations");
  if(res.status==cppunivariate::ZmgeomStatus::AllZero)Rcpp::warning("all observations are zero: base parameters are unidentified; supplied values are preserved.");
  if(res.status==cppunivariate::ZmgeomStatus::TwoPoint)Rcpp::warning("positives are all one: the attained two-point law is also fitted exactly by mlbinom(x, size = 1).");
  if(!std::isfinite(res.logLik))Rcpp::stop("MLE cannot be resolved at floating-point precision.");
  Rcpp::CharacterVector fixed(0);
  if(prob.isNotNull())fixed.push_back("prob");
  if(p0.isNotNull())fixed.push_back("p0");
  return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["prob"]=res.prob,Rcpp::_["p0"]=res.p0),
    Rcpp::_["logLik"]=res.logLik,Rcpp::_["iterations"]=res.iterations,
    Rcpp::_["fixed"]=fixed);
}
