#include "as_span.h"
#include "cppunivariate/distributions/dgamma/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>
// [[Rcpp::export]]
Rcpp::List mldgamma_(const Rcpp::NumericVector &x,
    Rcpp::Nullable<double> shape = R_NilValue,
    Rcpp::Nullable<double> rate = R_NilValue,
    double reltol = 1e-8, int iterlim = 400) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double a=shape.isNotNull() ? Rcpp::as<double>(shape):nan;
  const double b=rate.isNotNull() ? Rcpp::as<double>(rate):nan;
  if (shape.isNotNull() && (!std::isfinite(a) || a<=0)) Rcpp::stop("Invalid held `shape`.");
  if (rate.isNotNull() && (!std::isfinite(b) || b<=0)) Rcpp::stop("Invalid held `rate`.");
  const auto r=cppunivariate::dgamma_mle(dist_impl::as_span(x),reltol,iterlim,a,b);
  switch (r.status) {
    case cppunivariate::DgammaStatus::Ok: break;
    case cppunivariate::DgammaStatus::InvalidData: Rcpp::stop("Requires nonempty finite nonnegative integer data (absolute value below 2^52).");
    case cppunivariate::DgammaStatus::InvalidControl: Rcpp::stop("Invalid control: reltol must be finite and positive, iterlim at least 1.");
    case cppunivariate::DgammaStatus::PointMass: Rcpp::stop("The maximum likelihood estimator does not exist: the likelihood approaches a point mass.");
    case cppunivariate::DgammaStatus::TwoPointLimit:
      Rcpp::stop("The maximum likelihood estimator does not exist on two adjacent integers; use `mlbinom(x - k, size = 1)` with k = min(x).");
    case cppunivariate::DgammaStatus::IterationFailure: Rcpp::stop("MLE failed to converge; increase iterlim.");
  }
  Rcpp::CharacterVector fixed(0);
  if (shape.isNotNull()) fixed.push_back("shape");
  if (rate.isNotNull()) fixed.push_back("rate");
  return Rcpp::List::create(
    Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["shape"]=r.shape,Rcpp::_["rate"]=r.rate),
    Rcpp::_["logLik"]=r.logLik,Rcpp::_["iterations"]=r.iterations,Rcpp::_["fixed"]=fixed);
}
