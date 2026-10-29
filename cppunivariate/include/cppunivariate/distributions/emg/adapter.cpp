#include <Rcpp.h>
#include "as_span.h"
#include "cppunivariate/distributions/emg/mle.hpp"
// [[Rcpp::export]]
Rcpp::List mlemg_(const Rcpp::NumericVector &x,
    Rcpp::Nullable<double> mu=R_NilValue,
    Rcpp::Nullable<double> sigma=R_NilValue,
    Rcpp::Nullable<double> lambda=R_NilValue,
    double reltol=1e-8,int iterlim=500) {
  auto held=[](Rcpp::Nullable<double> a) {
    if(a.isNull())return cppunivariate::special::kNaN;
    const double v=Rcpp::as<double>(a);
    if(!std::isfinite(v))Rcpp::stop("Held parameters must be finite.");
    return v;
  };
  auto r=cppunivariate::emg_mle(dist_impl::as_span(x),reltol,iterlim,held(mu),held(sigma),held(lambda));
  if(r.status==cppunivariate::EmgStatus::NormalLimit)Rcpp::stop("The EMG MLE does not exist: normal face; use mlnorm for an unrestricted normal fit. The compared face retains held mu/sigma.");
  if(r.status==cppunivariate::EmgStatus::TranslatedExponentialLimit)Rcpp::stop("The EMG MLE does not exist: translated exponential face. No translated exponential fitter is available; for a free endpoint fit distances with mlexp(x - min(x)).");
  if(r.status==cppunivariate::EmgStatus::Unbounded)Rcpp::stop("The EMG likelihood is unbounded for these constant observations.");
  if(r.status==cppunivariate::EmgStatus::InvalidInput)Rcpp::stop("EMG requires finite data, finite mu, positive sigma and lambda, positive reltol and iterlim.");
  if(r.status==cppunivariate::EmgStatus::IterationFailure)Rcpp::stop("EMG search did not converge; increase iterlim.");
  Rcpp::CharacterVector fixed(0);
  if(mu.isNotNull())fixed.push_back("mu");
  if(sigma.isNotNull())fixed.push_back("sigma");
  if(lambda.isNotNull())fixed.push_back("lambda");
  return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["mu"]=r.mu,Rcpp::_["sigma"]=r.sigma,Rcpp::_["lambda"]=r.lambda),Rcpp::_["logLik"]=r.logLik,Rcpp::_["iterations"]=r.iterations,Rcpp::_["fixed"]=fixed);
}
