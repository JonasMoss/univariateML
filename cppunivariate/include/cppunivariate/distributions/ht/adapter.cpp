#include "as_span.h"
#include "cppunivariate/distributions/ht/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>
// [[Rcpp::export]]
Rcpp::List mlht_(const Rcpp::NumericVector &x, Rcpp::Nullable<double> nu = R_NilValue,
                 Rcpp::Nullable<double> sigma = R_NilValue) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double df=nu.isNotNull()?Rcpp::as<double>(nu):nan;
  const double sc=sigma.isNotNull()?Rcpp::as<double>(sigma):nan;
  if ((nu.isNotNull() && (!(df>0)||!std::isfinite(df))) ||
      (sigma.isNotNull() && (!(sc>0)||!std::isfinite(sc))))
    Rcpp::stop("Held `nu` and `sigma` must be positive finite numbers.");
  const auto r=cppunivariate::ht_mle(dist_impl::as_span(x),df,sc);
  if(r.status==cppunivariate::HtStatus::Unbounded)
    Rcpp::stop("Half-t likelihood is unbounded along sigma -> 0 with nu*(n-k) < k (k zeros); hold sigma or hold nu with nu*(n-k) > k.");
  if(r.status==cppunivariate::HtStatus::ScaleBoundary)
    Rcpp::stop("Half-t likelihood has a finite unattained supremum as sigma -> 0: nu*(n-k) = k.");
  if(r.status==cppunivariate::HtStatus::NormalLimit && sigma.isNotNull())
    Rcpp::stop("Half-t MLE escapes as nu -> infinity to the half-normal with sigma held. "
               "For a half-normal fit use `mlhnorm(x, mu = 0)`; that call estimates sigma.");
  if(r.status==cppunivariate::HtStatus::NormalLimit)
    Rcpp::stop("Half-t MLE escapes as nu -> infinity; use `mlhnorm(x, mu = 0)`.");
  if(r.status!=cppunivariate::HtStatus::Ok)
    Rcpp::stop("Half-t MLE failed: check finite nonnegative data and numerical search range.");
  Rcpp::CharacterVector fixed;
  if(nu.isNotNull()) fixed.push_back("nu");
  if(sigma.isNotNull()) fixed.push_back("sigma");
  return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(
    Rcpp::_["nu"]=r.nu,Rcpp::_["sigma"]=r.sigma),Rcpp::_["logLik"]=r.logLik,
    Rcpp::_["fixed"]=fixed,Rcpp::_["iterations"]=r.iterations);
}
