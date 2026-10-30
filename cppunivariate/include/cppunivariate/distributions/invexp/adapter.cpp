#include "as_span.h"
#include "cppunivariate/distributions/invexp/mle.hpp"
#include <Rcpp.h>
#include <limits>
#include <cmath>
// [[Rcpp::export]]
Rcpp::List mlinvexp_(const Rcpp::NumericVector &x,
    Rcpp::Nullable<double> scale = R_NilValue) {
  Rcpp::CharacterVector fixed(0);
  const double scale_held = scale.isNull() ? std::numeric_limits<double>::quiet_NaN() : Rcpp::as<double>(scale);
  if (scale.isNotNull()) {
    if (!(std::isfinite(scale_held) && scale_held > 0.0)) Rcpp::stop("Held `scale` must be positive and finite.");
    fixed.push_back("scale");
  }

  auto r=cppunivariate::invexp_mle(dist_impl::as_span(x), scale_held);
  if (!(r.scale>0.0) || !std::isfinite(r.scale) || !std::isfinite(r.logLik))
    Rcpp::stop("Inverse exponential MLE exceeds double precision.");
  return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["scale"]=r.scale), Rcpp::_["logLik"]=r.logLik, Rcpp::_["iterations"]=0, Rcpp::_["fixed"] = fixed);
}
