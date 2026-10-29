#include "as_span.h"
#include "cppunivariate/distributions/gev/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlgev_(const Rcpp::NumericVector &x,
                 Rcpp::Nullable<double> xi = R_NilValue,
                 double reltol = 1e-10, int iterlim = 100) {
  const double shape=xi.isNotNull() ? Rcpp::as<double>(xi) : std::numeric_limits<double>::quiet_NaN();
  if(xi.isNotNull() && (!std::isfinite(shape) || shape<-.5 || shape>1))
    Rcpp::stop("`xi` must be finite and in [-0.5, 1].");
  const auto r=cppunivariate::gev_mle(dist_impl::as_span(x),reltol,iterlim,shape);
  if(r.status==cppunivariate::GevStatus::InvalidData)
    Rcpp::stop("Generalized extreme value requires at least three finite observations, every tie multiplicity strictly less than half the sample size, finite range, positive finite `reltol` and positive `iterlim`.");
  if(r.status!=cppunivariate::GevStatus::Ok || !std::isfinite(r.logLik))
    Rcpp::stop("Generalized extreme value MLE failed to converge.");
  return Rcpp::List::create(
    Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["mu"]=r.mu,Rcpp::_["sigma"]=r.sigma,Rcpp::_["xi"]=r.xi),
    Rcpp::_["logLik"]=r.logLik,Rcpp::_["iterations"]=r.iterations,
    Rcpp::_["fixed"]=xi.isNotNull() ? Rcpp::CharacterVector::create("xi") : Rcpp::CharacterVector());
}
