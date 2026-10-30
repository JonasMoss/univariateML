#include <Rcpp.h>
// R shell: vector policy is shared with the other package-defined families.
#include "as_span.h"
#include "cppunivariate/distributions/yulesimon/mle.hpp"
#include "cppunivariate/distributions/yulesimon/dpqr.hpp"
// [[Rcpp::export]]
Rcpp::List mlyulesimon_(const Rcpp::IntegerVector &x,Rcpp::Nullable<double> rho=R_NilValue,double reltol=1e-10,int iterlim=200) {
  const double held=rho.isNull()?std::numeric_limits<double>::quiet_NaN():Rcpp::as<double>(rho);
  if (rho.isNotNull() && (!std::isfinite(held) || !(held>0)))
    Rcpp::stop("Invalid held `rho`.");
  const auto fit=cppunivariate::yulesimon_mle(dist_impl::as_ispan(x),reltol,iterlim,held);
  if (fit.status==cppunivariate::YulesimonStatus::PointMass)
    Rcpp::stop("The maximum likelihood estimator does not exist: all observations are one (point mass at one).");
  if (fit.status==cppunivariate::YulesimonStatus::InvalidInput)
    Rcpp::stop("Fitting requires positive integer observations, positive finite reltol and positive iterlim.");
  if (fit.status!=cppunivariate::YulesimonStatus::Ok)
    Rcpp::stop("Maximum likelihood search failed to converge; increase iterlim.");
  return Rcpp::List::create(
    Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["rho"]=fit.rho),
    Rcpp::_["logLik"]=fit.logLik,Rcpp::_["iterations"]=fit.iterations,
    Rcpp::_["fixed"]=rho.isNotNull()?Rcpp::CharacterVector::create("rho"):Rcpp::CharacterVector(0));
}
