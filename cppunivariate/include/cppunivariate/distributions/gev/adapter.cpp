#include "as_span.h"
#include "cppunivariate/distributions/gev/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlgev_(const Rcpp::NumericVector &x,
                 Rcpp::Nullable<double> xi = R_NilValue,
                 Rcpp::Nullable<double> mu = R_NilValue,
                 Rcpp::Nullable<double> sigma = R_NilValue,
                 double reltol = 1e-10, int iterlim = 100) {
  const double shape=xi.isNotNull() ? Rcpp::as<double>(xi) : std::numeric_limits<double>::quiet_NaN();
  if(xi.isNotNull() && (!std::isfinite(shape) || (shape < -1 && mu.isNull())))
    Rcpp::stop("`xi` must be finite and in [-1, 1].");
  const double location=mu.isNotNull() ? Rcpp::as<double>(mu) : std::numeric_limits<double>::quiet_NaN();
  const double scale=sigma.isNotNull() ? Rcpp::as<double>(sigma) : std::numeric_limits<double>::quiet_NaN();
  if(mu.isNotNull() && !std::isfinite(location)) Rcpp::stop("`mu` must be finite.");
  if(sigma.isNotNull() && (!(scale>0) || !std::isfinite(scale))) Rcpp::stop("`sigma` must be positive and finite.");
  Rcpp::CharacterVector fixed;
  if(mu.isNotNull()) fixed.push_back("mu");
  if(sigma.isNotNull()) fixed.push_back("sigma");
  if(xi.isNotNull()) fixed.push_back("xi");
  const auto r=cppunivariate::gev_mle(dist_impl::as_span(x),reltol,iterlim,shape,location,scale);
  if(r.status==cppunivariate::GevStatus::NoMLE)
    Rcpp::stop("Generalized extreme value has no finite maximum: minimum multiplicity permits scale collapse at held mu.");
  if(r.status==cppunivariate::GevStatus::InvalidData)
    Rcpp::stop("Generalized extreme value requires at least three finite observations, an admissible multiplicity at the minimum, finite range, compatible support and holds, positive finite `reltol` and positive `iterlim`.");
  if(r.status!=cppunivariate::GevStatus::Ok || !std::isfinite(r.logLik))
    Rcpp::stop("Generalized extreme value MLE failed to converge.");
  Rcpp::CharacterVector restricted;
  if(r.restricted & 1u) restricted.push_back("lower","mu");
  if(r.restricted & 2u) restricted.push_back("upper","mu");
  if(r.restricted & 4u) restricted.push_back("lower","sigma");
  if(r.restricted & 8u) restricted.push_back("upper","sigma");
  if(r.restricted & 16u) restricted.push_back("lower","xi");
  if(r.restricted & 32u) restricted.push_back("upper","xi");
  return Rcpp::List::create(
    Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["mu"]=r.mu,Rcpp::_["sigma"]=r.sigma,Rcpp::_["xi"]=r.xi),
    Rcpp::_["logLik"]=r.logLik,Rcpp::_["iterations"]=r.iterations,
    Rcpp::_["restricted"]=restricted, Rcpp::_["fixed"]=fixed);
}
