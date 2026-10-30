#include "as_span.h"
#include "cppunivariate/distributions/frechet/mle.hpp"
#include <Rcpp.h>
// [[Rcpp::export]]
Rcpp::List mlfrechet_(const Rcpp::NumericVector &x,
 Rcpp::Nullable<double> mu,
 Rcpp::Nullable<double> lambda=R_NilValue,
 Rcpp::Nullable<double> sigma=R_NilValue,
 double reltol=1e-10, int iterlim=100) {
 if(mu.isNotNull() && !std::isfinite(Rcpp::as<double>(mu))) Rcpp::stop("mu must be finite.");
 const double shape=lambda.isNotNull() ? Rcpp::as<double>(lambda) : std::numeric_limits<double>::quiet_NaN();
 const double scale=sigma.isNotNull() ? Rcpp::as<double>(sigma) : std::numeric_limits<double>::quiet_NaN();
 if(lambda.isNotNull() && (!(shape>0) || !std::isfinite(shape))) Rcpp::stop("lambda must be positive and finite.");
 if(sigma.isNotNull() && (!(scale>0) || !std::isfinite(scale))) Rcpp::stop("sigma must be positive and finite.");
 Rcpp::CharacterVector fixed;
 if(lambda.isNotNull()) fixed.push_back("lambda");
 if(mu.isNotNull()) fixed.push_back("mu");
 if(sigma.isNotNull()) fixed.push_back("sigma");
 const auto r=cppunivariate::frechet_mle(dist_impl::as_span(x),reltol,iterlim,
 mu.isNotNull() ? Rcpp::as<double>(mu) : std::numeric_limits<double>::quiet_NaN(),shape,scale);
 if(r.status==cppunivariate::FrechetStatus::GumbelLimit) Rcpp::stop("Frechet selects the Gumbel limit. Use `mlgumbel`.");
 if(r.status==cppunivariate::FrechetStatus::NoMLE) Rcpp::stop("Frechet has no finite maximum: minimum multiplicity permits scale collapse.");
 if(r.status==cppunivariate::FrechetStatus::InvalidData) Rcpp::stop("Frechet requires finite observations strictly above held mu and compatible holds. Free shape and scale require nonconstant data; the default free-mu fit requires at least three observations and admissible minimum tie multiplicity. Constant data can have an unbounded shape likelihood and fixed shape can have an unattained collapse face. Tolerance and iteration limit must be positive.");
 if(r.status!=cppunivariate::FrechetStatus::Ok) Rcpp::stop("Frechet profile search failed to converge.");
  Rcpp::CharacterVector restricted;
  if(r.restricted & 1u) restricted.push_back("lower","lambda");
  if(r.restricted & 2u) restricted.push_back("upper","lambda");
  if(r.restricted & 4u) restricted.push_back("lower","mu");
  if(r.restricted & 8u) restricted.push_back("upper","mu");
  if(r.restricted & 16u) restricted.push_back("lower","sigma");
  if(r.restricted & 32u) restricted.push_back("upper","sigma");
 return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(
 Rcpp::_["lambda"]=r.lambda,Rcpp::_["mu"]=r.mu,Rcpp::_["sigma"]=r.sigma),
 Rcpp::_["logLik"]=r.logLik,Rcpp::_["iterations"]=r.iterations,
 Rcpp::_["restricted"]=restricted, Rcpp::_["fixed"]=fixed);
}
