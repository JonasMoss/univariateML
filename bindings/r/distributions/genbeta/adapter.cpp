#include "as_span.h"
#include "cppunivariate/distributions/genbeta/mle.hpp"
#include <Rcpp.h>
// [[Rcpp::export]]
Rcpp::List mlgenbeta_(const Rcpp::NumericVector &x, double scale,
    double gradtol = 1e-7, int iterlim = 300,
    Rcpp::Nullable<double> shape1 = R_NilValue,
    Rcpp::Nullable<double> shape2 = R_NilValue,
    Rcpp::Nullable<double> shape3 = R_NilValue) {
  if(!(gradtol>0)||!std::isfinite(gradtol)||iterlim<1)
    Rcpp::stop("`gradtol` must be positive and finite and `iterlim` must be positive.");
  Rcpp::CharacterVector fixed;
  auto held=[&](Rcpp::Nullable<double> v,const char *name) {
    if(v.isNull())return std::numeric_limits<double>::quiet_NaN();
    const double z=Rcpp::as<double>(v);
    if(!(z>0)||!std::isfinite(z))Rcpp::stop("`%s` must be a positive finite number.",name);
    fixed.push_back(name);return z;
  };
  const double a=held(shape1,"shape1"),b=held(shape2,"shape2"),t=held(shape3,"shape3");
  if(b==1&&std::isnan(a)&&std::isnan(t))fixed.push_back("shape3");
  fixed.push_back("scale");
  const auto r=cppunivariate::genbeta_mle(dist_impl::as_span(x),scale,gradtol,iterlim,a,b,t);
  switch(r.status) {
    case cppunivariate::GenbetaStatus::Ok:break;
    case cppunivariate::GenbetaStatus::UnitGammaBoundary:
      if(!std::isnan(b))Rcpp::stop("Generalized beta selects the unit-gamma limit with shapelog held at shape2. `mllgamma(scale / x)` fits the unrestricted loggamma; holding shapelog there is not supported.");
      Rcpp::stop("Generalized beta selects the unit-gamma limit. Use `mllgamma(scale / x)`.");
    case cppunivariate::GenbetaStatus::PowerBoundary:
      if(!std::isnan(b))Rcpp::stop("Generalized beta selects power with the endpoint held: use `mlgenbeta(x, scale = scale, shape2 = 1)`. `mlpower` estimates its endpoint.");
      Rcpp::stop("Generalized beta selects a limiting power law. Use `mlpower`.");
    case cppunivariate::GenbetaStatus::InvalidData:Rcpp::stop("Generalized beta requires observations strictly between 0 and scale; constant samples with free beta shapes are not supported (the unrestricted likelihood is unbounded).");
    case cppunivariate::GenbetaStatus::InvalidFixed:Rcpp::stop("Generalized beta held parameters must be positive finite numbers.");
    case cppunivariate::GenbetaStatus::SearchUnresolved:Rcpp::stop("Generalized beta search unresolved; increase iterlim or hold shapes.");
  }
  return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(
    Rcpp::_["shape1"]=r.shape1,Rcpp::_["shape2"]=r.shape2,
    Rcpp::_["shape3"]=r.shape3,Rcpp::_["scale"]=r.scale),
    Rcpp::_["logLik"]=r.logLik,Rcpp::_["iterations"]=r.iterations,
    Rcpp::_["fixed"]=fixed,Rcpp::_["support"]=Rcpp::NumericVector::create(0,scale));
}
