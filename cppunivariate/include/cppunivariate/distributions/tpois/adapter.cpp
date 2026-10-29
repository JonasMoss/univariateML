#include "as_span.h"
#include "cppunivariate/distributions/tpois/mle.hpp"
#include <Rcpp.h>
// [[Rcpp::export]]
Rcpp::List mltpois_(const Rcpp::IntegerVector &x, double a, double b, Rcpp::Nullable<double> lambda=R_NilValue) {
  const double held=lambda.isNull()?std::numeric_limits<double>::quiet_NaN():Rcpp::as<double>(lambda);
  if(lambda.isNotNull()&&std::isnan(held))Rcpp::stop("Fixed parameter must not be missing.");
  auto r=cppunivariate::tpois_mle(dist_impl::as_ispan(x),a,b,held);
  if(r.status==cppunivariate::truncated_detail::Status::LowerPointMass) Rcpp::stop("MLE escapes to a point mass at the lowest support point.");
  if(r.status==cppunivariate::truncated_detail::Status::UpperPointMass) Rcpp::stop("MLE escapes to a point mass at the highest support point.");
  if(r.status==cppunivariate::truncated_detail::Status::Unidentified) Rcpp::stop("Parameter is unidentified on singleton truncation support.");
  if(r.status==cppunivariate::truncated_detail::Status::InvalidInput) Rcpp::stop("Invalid design parameters or observations outside truncation bounds a < x <= b.");
  if(r.status!=cppunivariate::truncated_detail::Status::Ok) Rcpp::stop("Truncated normalizer cannot be resolved within numerical precision or work budget.");
  Rcpp::CharacterVector fixed=Rcpp::CharacterVector::create("a","b");
  if(lambda.isNotNull())fixed.push_back("lambda");
  return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["lambda"]=r.parameter, Rcpp::_["a"]=a, Rcpp::_["b"]=b),Rcpp::_["logLik"]=r.logLik,Rcpp::_["iterations"]=r.iterations,
    Rcpp::_["fixed"]=fixed);
}
