#include "as_span.h"
#include "cppunivariate/distributions/dlaplace/mle.hpp"
#include <Rcpp.h>
// [[Rcpp::export]]
Rcpp::List mldlaplace_(const Rcpp::IntegerVector &x, Rcpp::Nullable<double> location=R_NilValue, Rcpp::Nullable<double> scale=R_NilValue) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double mu=location.isNull()?nan:Rcpp::as<double>(location), p=scale.isNull()?nan:Rcpp::as<double>(scale);
  if((location.isNotNull()&&std::isnan(mu))||(scale.isNotNull()&&std::isnan(p)))Rcpp::stop("Fixed parameters must not be missing.");
  auto r=cppunivariate::dlaplace_mle(dist_impl::as_ispan(x),mu,p);
  if(r.status==cppunivariate::DlaplaceStatus::PointMass) Rcpp::stop("MLE escapes to a point mass (scale tends to zero).");
  if(r.status!=cppunivariate::DlaplaceStatus::Ok)Rcpp::stop("Require integer location and scale strictly between zero and one.");
  if(r.not_unique)Rcpp::warning("Discrete Laplace location MLE is not unique; returning the smallest integer median.");
  Rcpp::CharacterVector fixed(0);if(location.isNotNull())fixed.push_back("location");if(scale.isNotNull())fixed.push_back("scale");
  return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["location"]=r.location,Rcpp::_["scale"]=r.scale),Rcpp::_["logLik"]=r.logLik,Rcpp::_["fixed"]=fixed);
}
