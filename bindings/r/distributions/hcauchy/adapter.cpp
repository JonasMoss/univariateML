#include "as_span.h"
#include "cppunivariate/distributions/hcauchy/mle.hpp"
#include <Rcpp.h>
// [[Rcpp::export]]
Rcpp::List mlhcauchy_(const Rcpp::NumericVector &x, Rcpp::Nullable<double> mu,
                       Rcpp::Nullable<double> sigma = R_NilValue) {
  const bool mu_held = mu.isNotNull();
  const double held_mu = mu_held ? Rcpp::as<double>(mu) : std::numeric_limits<double>::quiet_NaN();
  if (mu_held && !std::isfinite(held_mu)) Rcpp::stop("`mu` must be finite.");
  const double sc=sigma.isNotNull()?Rcpp::as<double>(sigma):std::numeric_limits<double>::quiet_NaN();
  if (sigma.isNotNull() && (!(sc>0)||!std::isfinite(sc))) Rcpp::stop("`sigma` must be positive finite.");
  const auto r=cppunivariate::hcauchy_mle(dist_impl::as_span(x),held_mu,sc);
  if(r.status==cppunivariate::HcauchyStatus::Unbounded)
    Rcpp::stop("Half-Cauchy likelihood is unbounded as sigma -> 0 when more than half the observations equal the free or held endpoint (including all equal to it).");
  if(r.status==cppunivariate::HcauchyStatus::ScaleBoundary)
    Rcpp::stop("Half-Cauchy likelihood has a finite unattained supremum as sigma -> 0 when exactly half the observations equal the free or held endpoint.");
  if(mu_held && r.status==cppunivariate::HcauchyStatus::InvalidData)
    Rcpp::stop("Half-Cauchy observations must be finite and at least the held mu.");
  if(r.status!=cppunivariate::HcauchyStatus::Ok)
    Rcpp::stop("Half-Cauchy MLE requires finite data and a representable positive scale.");
  Rcpp::CharacterVector fixed;
  if(mu_held) fixed.push_back("mu");
  if(sigma.isNotNull()) fixed.push_back("sigma");
  return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(
    Rcpp::_["mu"]=r.mu,Rcpp::_["sigma"]=r.sigma),Rcpp::_["logLik"]=r.logLik,
    Rcpp::_["fixed"]=fixed);
}
