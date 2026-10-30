#include <Rcpp.h>
#include "as_span.h"
#include "cppunivariate/distributions/skellam/mle.hpp"
// [[Rcpp::export]]
Rcpp::List mlskellam_(const Rcpp::NumericVector& x,
    Rcpp::Nullable<double> mu1=R_NilValue,Rcpp::Nullable<double> mu2=R_NilValue,
    double reltol=1e-9,int iterlim=150) {
  const double a=mu1.isNotNull()?Rcpp::as<double>(mu1):R_NaN;
  const double b=mu2.isNotNull()?Rcpp::as<double>(mu2):R_NaN;
  if((mu1.isNotNull() && !(std::isfinite(a) && a>=0)) ||
     (mu2.isNotNull() && !(std::isfinite(b) && b>=0))) Rcpp::stop("Fixed rates must be finite and nonnegative.");
  const auto fit=cppunivariate::skellam_mle(dist_impl::as_span(x),reltol,iterlim,a,b);
  if(fit.status==cppunivariate::SkellamStatus::InvalidInput) Rcpp::stop("Skellam requires finite integer observations, positive reltol and positive iterlim.");
  if(fit.status==cppunivariate::SkellamStatus::Impossible) Rcpp::stop("The fixed rates give zero likelihood for these observations.");
  if(fit.status!=cppunivariate::SkellamStatus::Ok) Rcpp::stop("Skellam search did not converge; increase iterlim.");
  Rcpp::CharacterVector fixed;
  if(mu1.isNotNull()) fixed.push_back("mu1");
  if(mu2.isNotNull()) fixed.push_back("mu2");
  return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["mu1"]=fit.mu1,Rcpp::_["mu2"]=fit.mu2),Rcpp::_["logLik"]=fit.logLik,Rcpp::_["iterations"]=fit.iterations,Rcpp::_["fixed"]=fixed);
}
