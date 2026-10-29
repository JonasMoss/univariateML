#include "as_span.h"
#include "cppunivariate/distributions/pareto3/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <algorithm>
// [[Rcpp::export]]
Rcpp::List mlpareto3_(const Rcpp::NumericVector &x, double min,
 double reltol = 1e-10, int iterlim = 1000, Rcpp::Nullable<double> shape = R_NilValue, Rcpp::Nullable<double> scale = R_NilValue) {
 if(shape.isNotNull() && (!(Rcpp::as<double>(shape)>0)||!std::isfinite(Rcpp::as<double>(shape)))) Rcpp::stop("`shape` must be positive and finite.");
 if(scale.isNotNull() && (!(Rcpp::as<double>(scale)>0)||!std::isfinite(Rcpp::as<double>(scale)))) Rcpp::stop("`scale` must be positive and finite.");

 if (!std::isfinite(min)) Rcpp::stop("Held bounds must be finite and ordered.");
 for(double v:x) if (!(v>min)) Rcpp::stop("Observations must lie strictly inside the held support.");
 Rcpp::CharacterVector fixed=Rcpp::CharacterVector::create("min");
 if(shape.isNotNull()) fixed.push_back("shape");
 if(scale.isNotNull()) fixed.push_back("scale");
 auto res=cppunivariate::pareto3_mle(dist_impl::as_span(x),min,reltol,iterlim,shape.isNotNull()?Rcpp::as<double>(shape):NA_REAL,scale.isNotNull()?Rcpp::as<double>(scale):NA_REAL);
  if(res.status==cppunivariate::Pareto3Status::IterationFailure) Rcpp::stop("Pareto III delegate search failed to converge.");
  if (!(res.shape>0.0) || !((1/res.scale)>0.0) || !std::isfinite(res.shape) || !std::isfinite(res.scale) || !(res.scale>0) || !std::isfinite(res.logLik)) {
    if (std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}))
      Rcpp::stop("The log-logistic maximum likelihood estimator does not exist for constant data: the likelihood is unbounded.");
    Rcpp::stop("Log-logistic MLE failed: data exceed double precision.");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(Rcpp::_["shape"] = res.shape, Rcpp::_["scale"] = res.scale, Rcpp::_["min"] = min),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations, Rcpp::_["fixed"] = fixed);
}
