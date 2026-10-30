#include "as_span.h"
#include "cppunivariate/distributions/gb2/mle.hpp"
#include <Rcpp.h>
// [[Rcpp::export]]
Rcpp::List mlgb2_(const Rcpp::NumericVector &x, double gradtol = 1e-7,
    int iterlim = 300, Rcpp::Nullable<double> shape1 = R_NilValue,
    Rcpp::Nullable<double> shape2 = R_NilValue,
    Rcpp::Nullable<double> shape3 = R_NilValue,
    Rcpp::Nullable<double> scale = R_NilValue) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  Rcpp::CharacterVector fixed;
  auto held=[&](Rcpp::Nullable<double> v, const char *name) {
    if(v.isNull())return nan;
    const double z=Rcpp::as<double>(v);
    if(!(z>0)||!std::isfinite(z)) Rcpp::stop("`%s` must be a positive finite number.",name);
    fixed.push_back(name);return z;
  };
  const double q=held(shape1,"shape1"),a=held(shape2,"shape2"),
    p=held(shape3,"shape3"),b=held(scale,"scale");
  const auto r=cppunivariate::gb2_mle(dist_impl::as_span(x),gradtol,iterlim,q,a,p,b);
  switch(r.status) {
    case cppunivariate::Gb2Status::Ok: break;
    case cppunivariate::Gb2Status::TrGammaBoundary: Rcpp::stop("GB2 selects a limiting model. Use `mltrgamma`.");
    case cppunivariate::Gb2Status::InvTrGammaBoundary: Rcpp::stop("GB2 selects a limiting model. Use `mlinvtrgamma`.");
    case cppunivariate::Gb2Status::LognormalBoundary: Rcpp::stop("GB2 selects a limiting model. Use `mllnorm`.");
    case cppunivariate::Gb2Status::ParetoBoundary: Rcpp::stop("GB2 selects a limiting model. Use `mlpareto`.");
    case cppunivariate::Gb2Status::PowerBoundary: Rcpp::stop("GB2 selects a limiting model. Use `mlpower`.");
    case cppunivariate::Gb2Status::DoubleParetoBoundary: Rcpp::stop("GB2 selects the double Pareto limit; this package has no double Pareto fitter.");
    case cppunivariate::Gb2Status::InvalidData: Rcpp::stop("GB2 requires positive finite nonconstant observations; constant data have an unbounded free likelihood.");
    case cppunivariate::Gb2Status::InvalidFixed: Rcpp::stop("GB2 fixed parameters must be positive and finite.");
    case cppunivariate::Gb2Status::ScaleOutOfRange: Rcpp::stop("GB2 scale is outside the representable range.");
    default: Rcpp::stop("GB2 search unresolved: no verified interior maximum; increase iterlim or inspect the limiting models.");
  }
  return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(
    Rcpp::_["shape1"]=r.shape1,Rcpp::_["shape2"]=r.shape2,
    Rcpp::_["shape3"]=r.shape3,Rcpp::_["scale"]=r.scale),
    Rcpp::_["logLik"]=r.logLik,Rcpp::_["iterations"]=r.iterations,Rcpp::_["fixed"]=fixed);
}
