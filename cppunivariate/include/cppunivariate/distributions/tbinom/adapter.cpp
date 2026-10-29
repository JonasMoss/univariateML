#include "as_span.h"
#include "cppunivariate/distributions/tbinom/mle.hpp"
#include <Rcpp.h>
// [[Rcpp::export]]
Rcpp::List mltbinom_(const Rcpp::IntegerVector &x, double a, double b, double size, Rcpp::Nullable<double> prob=R_NilValue) {
  const double held=prob.isNull()?std::numeric_limits<double>::quiet_NaN():Rcpp::as<double>(prob);
  if(prob.isNotNull()&&std::isnan(held))Rcpp::stop("Fixed parameter must not be missing.");
  auto r=cppunivariate::tbinom_mle(dist_impl::as_ispan(x),a,b,size,held);
  if(r.status==cppunivariate::truncated_detail::Status::LowerPointMass) Rcpp::stop("MLE escapes to a point mass at the lowest support point.");
  if(r.status==cppunivariate::truncated_detail::Status::UpperPointMass) Rcpp::stop("MLE escapes to a point mass at the highest support point.");
  if(r.status==cppunivariate::truncated_detail::Status::Unidentified) Rcpp::stop("Parameter is unidentified on singleton truncation support.");
  if(r.status==cppunivariate::truncated_detail::Status::InvalidInput) Rcpp::stop("Invalid design parameters or observations outside truncation bounds a < x <= b.");
  if(r.status!=cppunivariate::truncated_detail::Status::Ok) Rcpp::stop("Truncated normalizer cannot be resolved within numerical precision or work budget.");
  Rcpp::CharacterVector fixed=Rcpp::CharacterVector::create("a","b","size");
  if(prob.isNotNull())fixed.push_back("prob");
  return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["prob"]=r.parameter, Rcpp::_["a"]=a, Rcpp::_["b"]=b, Rcpp::_["size"]=size),Rcpp::_["logLik"]=r.logLik,Rcpp::_["iterations"]=r.iterations,
    Rcpp::_["fixed"]=fixed);
}
