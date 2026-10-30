#include "as_span.h"
#include "cppunivariate/distributions/fpareto/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <algorithm>
// [[Rcpp::export]]
Rcpp::List mlfpareto_(const Rcpp::NumericVector &x, double min,
 double reltol = 1e-7, int iterlim = 1000, Rcpp::Nullable<double> shape1 = R_NilValue, Rcpp::Nullable<double> shape2 = R_NilValue, Rcpp::Nullable<double> shape3 = R_NilValue, Rcpp::Nullable<double> scale = R_NilValue) {
 if (!std::isfinite(min)) Rcpp::stop("Held bounds must be finite and ordered.");
 for(double v:x) if (!(v>min)) Rcpp::stop("Observations must lie strictly inside the held support.");
 Rcpp::CharacterVector fixed=Rcpp::CharacterVector::create("min");
 if(shape1.isNotNull()) { if(!(Rcpp::as<double>(shape1)>0) || !std::isfinite(Rcpp::as<double>(shape1))) Rcpp::stop("Held parameters must be positive and finite."); fixed.push_back("shape1"); }
 if(shape2.isNotNull()) { if(!(Rcpp::as<double>(shape2)>0) || !std::isfinite(Rcpp::as<double>(shape2))) Rcpp::stop("Held parameters must be positive and finite."); fixed.push_back("shape2"); }
 if(shape3.isNotNull()) { if(!(Rcpp::as<double>(shape3)>0) || !std::isfinite(Rcpp::as<double>(shape3))) Rcpp::stop("Held parameters must be positive and finite."); fixed.push_back("shape3"); }
 if(scale.isNotNull()) { if(!(Rcpp::as<double>(scale)>0) || !std::isfinite(Rcpp::as<double>(scale))) Rcpp::stop("Held parameters must be positive and finite."); fixed.push_back("scale"); }
 auto res=cppunivariate::fpareto_mle(dist_impl::as_span(x),min,reltol,iterlim,shape1.isNotNull() ? Rcpp::as<double>(shape1) : std::numeric_limits<double>::quiet_NaN(),shape2.isNotNull() ? Rcpp::as<double>(shape2) : std::numeric_limits<double>::quiet_NaN(),shape3.isNotNull() ? Rcpp::as<double>(shape3) : std::numeric_limits<double>::quiet_NaN(),scale.isNotNull() ? Rcpp::as<double>(scale) : std::numeric_limits<double>::quiet_NaN());
  switch(res.status) {
    case cppunivariate::Gb2Status::Ok: break;
    case cppunivariate::Gb2Status::TrGammaBoundary: Rcpp::stop("GB2 selects a limiting model. Use `mltrgamma(x - min)`.");
    case cppunivariate::Gb2Status::InvTrGammaBoundary: Rcpp::stop("GB2 selects a limiting model. Use `mlinvtrgamma(x - min)`.");
    case cppunivariate::Gb2Status::LognormalBoundary: Rcpp::stop("GB2 selects a limiting model. Use `mllnorm(x - min)`.");
    case cppunivariate::Gb2Status::ParetoBoundary:
      if(scale.isNotNull()) Rcpp::stop("Feller-Pareto selects a shifted Pareto with its endpoint held at min + scale; no held-endpoint Pareto fitter is available.");
      Rcpp::stop("GB2 selects a limiting model. Use `mlpareto(x - min)`.");
    case cppunivariate::Gb2Status::PowerBoundary:
      if(scale.isNotNull()) Rcpp::stop("Feller-Pareto selects a shifted power distribution on (min, min + scale) with both endpoints held; no fitter for this held-upper-endpoint limit is available.");
      Rcpp::stop("GB2 selects a limiting model. Use `mlpower(x - min)`.");
    case cppunivariate::Gb2Status::DoubleParetoBoundary: Rcpp::stop("GB2 selects the double Pareto limit; this package has no double Pareto fitter.");
    case cppunivariate::Gb2Status::InvalidData: Rcpp::stop("GB2 requires positive finite nonconstant observations; constant data have an unbounded free likelihood.");
    case cppunivariate::Gb2Status::InvalidFixed: Rcpp::stop("GB2 fixed parameters must be positive and finite.");
    case cppunivariate::Gb2Status::ScaleOutOfRange: Rcpp::stop("GB2 scale is outside the representable range.");
    default: Rcpp::stop("GB2 search unresolved: no verified interior maximum; increase iterlim or inspect the limiting models.");
  }
  return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["shape1"] = res.shape1, Rcpp::_["shape2"] = res.shape2, Rcpp::_["shape3"] = res.shape3, Rcpp::_["scale"] = res.scale, Rcpp::_["min"] = min),
    Rcpp::_["logLik"]=res.logLik,Rcpp::_["iterations"]=res.iterations,Rcpp::_["fixed"]=fixed);
}
