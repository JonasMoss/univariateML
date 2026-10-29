#include "as_span.h"
#include "cppunivariate/distributions/dnorm/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>
// [[Rcpp::export]]
Rcpp::List mldnorm_(const Rcpp::NumericVector &x,
    Rcpp::Nullable<double> mean = R_NilValue,
    Rcpp::Nullable<double> sd = R_NilValue,
    double reltol = 1e-9, int iterlim = 200) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double a=mean.isNotNull() ? Rcpp::as<double>(mean):nan;
  const double b=sd.isNotNull() ? Rcpp::as<double>(sd):nan;
  if (mean.isNotNull() && (!std::isfinite(a))) Rcpp::stop("Invalid held `mean`.");
  if (sd.isNotNull() && (!std::isfinite(b) || b<=0)) Rcpp::stop("Invalid held `sd`.");
  const auto r=cppunivariate::dnorm_mle(dist_impl::as_span(x),reltol,iterlim,a,b);
  switch (r.status) {
    case cppunivariate::DnormStatus::Ok: break;
    case cppunivariate::DnormStatus::InvalidData: Rcpp::stop("Requires nonempty finite integer data (absolute value below 2^52).");
    case cppunivariate::DnormStatus::InvalidControl: Rcpp::stop("Invalid control: reltol must be finite and positive, iterlim at least 1.");
    case cppunivariate::DnormStatus::PointMass: Rcpp::stop("The maximum likelihood estimator does not exist: the likelihood approaches a point mass.");
    case cppunivariate::DnormStatus::TwoPointLimit:
      Rcpp::stop("The maximum likelihood estimator does not exist on two adjacent integers; use `mlbinom(x - k, size = 1)` with k = min(x).");
    case cppunivariate::DnormStatus::ScaleLimit: Rcpp::stop("With mean held the likelihood approaches its zero-scale limit; the maximum likelihood estimator does not exist.");
    case cppunivariate::DnormStatus::IterationFailure: Rcpp::stop("MLE failed to converge; increase iterlim.");
  }
  Rcpp::CharacterVector fixed(0);
  if (mean.isNotNull()) fixed.push_back("mean");
  if (sd.isNotNull()) fixed.push_back("sd");
  return Rcpp::List::create(
    Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["mean"]=r.mean,Rcpp::_["sd"]=r.sd),
    Rcpp::_["logLik"]=r.logLik,Rcpp::_["iterations"]=r.iterations,Rcpp::_["fixed"]=fixed);
}
