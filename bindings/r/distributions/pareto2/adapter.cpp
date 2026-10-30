#include "as_span.h"
#include "cppunivariate/distributions/pareto2/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <algorithm>
// [[Rcpp::export]]
Rcpp::List mlpareto2_(const Rcpp::NumericVector &x, double min,
 double reltol = 1e-10, int iterlim = 1000, Rcpp::Nullable<double> shape = R_NilValue, Rcpp::Nullable<double> scale = R_NilValue) {
 if(shape.isNotNull() && (!(Rcpp::as<double>(shape)>0)||!std::isfinite(Rcpp::as<double>(shape)))) Rcpp::stop("`shape` must be positive and finite.");
 if(scale.isNotNull() && (!(Rcpp::as<double>(scale)>0)||!std::isfinite(Rcpp::as<double>(scale)))) Rcpp::stop("`scale` must be positive and finite.");

 if (!std::isfinite(min)) Rcpp::stop("Held bounds must be finite and ordered.");
 for(double v:x) if (!(v>min)) Rcpp::stop("Observations must lie strictly inside the held support.");
 Rcpp::CharacterVector fixed=Rcpp::CharacterVector::create("min");
 if(shape.isNotNull()) fixed.push_back("shape");
 if(scale.isNotNull()) fixed.push_back("scale");
 auto res=cppunivariate::pareto2_mle(dist_impl::as_span(x),min,reltol,iterlim,shape.isNotNull()?Rcpp::as<double>(shape):NA_REAL,scale.isNotNull()?Rcpp::as<double>(scale):NA_REAL);
  if (res.status == cppunivariate::LomaxStatus::ExponentialLimit) {
    Rcpp::stop("The Lomax maximum likelihood estimator does not exist for data "
               "whose likelihood is maximized on the exponential face. Use `mlexp(x - min)` to fit an "
               "exponential distribution.");
  }
  if (res.status != cppunivariate::LomaxStatus::Ok || !std::isfinite(res.scale) || !(res.scale>0) ||
      !std::isfinite(res.shape)) {
    Rcpp::stop("Lomax MLE failed to converge (check dispersion/inputs)");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(Rcpp::_["shape"] = res.shape, Rcpp::_["scale"] = res.scale, Rcpp::_["min"] = min),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations, Rcpp::_["fixed"] = fixed);
}
