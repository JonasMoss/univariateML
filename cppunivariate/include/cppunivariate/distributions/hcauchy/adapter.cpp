#include "as_span.h"
#include "cppunivariate/distributions/hcauchy/mle.hpp"
#include <Rcpp.h>
// [[Rcpp::export]]
Rcpp::List mlhcauchy_(const Rcpp::NumericVector &x) {
  const auto r=cppunivariate::hcauchy_mle(dist_impl::as_span(x));
  if(r.status==cppunivariate::HcauchyStatus::Unbounded)
    Rcpp::stop("Half-Cauchy likelihood is unbounded as sigma -> 0 when more than half the observations are zero (including all-zero data).");
  if(r.status==cppunivariate::HcauchyStatus::ScaleBoundary)
    Rcpp::stop("Half-Cauchy likelihood has a finite unattained supremum as sigma -> 0 when exactly half the observations are zero.");
  if(r.status!=cppunivariate::HcauchyStatus::Ok)
    Rcpp::stop("Half-Cauchy MLE requires finite nonnegative data and a representable positive scale.");
  return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["sigma"]=r.sigma),Rcpp::_["logLik"]=r.logLik);
}
